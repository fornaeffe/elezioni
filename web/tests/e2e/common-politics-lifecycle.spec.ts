import { expect, test, type BrowserContext, type Page } from '@playwright/test';
import { commonPoliticsFixture } from '../fixtures/common-politics-input';
import { politicsDraftKey } from '../../src/lib/politics/files/draft';
import { politicsSessionsKey } from '../../src/lib/politics/workspace-controller';
import { REFORM, ROSATELLUM, type PoliticsScenario } from '../../src/lib/politics/scenario/schema';
import { decodeScenario } from '../../src/lib/politics/scenario/serialization';
import { readFile } from 'node:fs/promises';
import { generatedDefaultRegionalErScenario } from '../../src/lib/scenario/regional-er-defaults.generated';

const base = process.env.BASE_PATH ?? '';
test.setTimeout(120_000);
async function setup(context: BrowserContext, page: Page, law: typeof REFORM | typeof ROSATELLUM, model: 'M01' | 'M02') {
  const f = await commonPoliticsFixture(law, model);
  f.scenario.execution.simulations = 3; f.scenario.execution.workers = { kind: 'fixed', count: 1 };
  await context.route('**/constructed/*.json', route => {
    const id = decodeURIComponent(new URL(route.request().url()).pathname.split('/').at(-1)!.replace(/\.json$/, ''));
    const bytes = f.bytes.get(id);
    return bytes ? route.fulfill({ body: Buffer.from(bytes), contentType: 'application/json' }) : route.abort();
  });
  await context.route('**/data/reform-2026/fast-share/*.gz', route => {
    const name = new URL(route.request().url()).pathname.split('/').at(-1)!;
    const bytes = f.original.files.get(name);
    return bytes ? route.fulfill({ body: Buffer.from(bytes), contentType: 'application/octet-stream' }) : route.continue();
  });
  await page.goto(`${base}/`);
  await page.evaluate(({ key, scenario }) => localStorage.setItem(key, JSON.stringify(scenario)), { key: politicsDraftKey, scenario: f.scenario });
  await page.goto(`${base}/politics`);
  await expect(page.locator('.editor-status summary')).toContainText('Modello pronto', { timeout: 60000 });
  return f;
}
async function saved(page: Page) {
  return page.evaluate(async ({ key }) => {
    const index = JSON.parse(localStorage.getItem(key)!);
    const db = await new Promise<IDBDatabase>((resolve, reject) => { const r = indexedDB.open('elezioni-result-chunks-v1'); r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); });
    try { return await new Promise<any>((resolve, reject) => { const r = db.transaction('results').objectStore('results').get(index.selected); r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); }); }
    finally { db.close(); }
  }, { key: politicsSessionsKey });
}
async function run(page: Page) {
  await page.getByRole('button', { name: 'Avvia simulazione', exact: true }).click();
  await expect(page.locator('.editor-status summary')).toContainText(/Esecuzione completata|Esecuzione interrotta/, { timeout: 60000 });
  await expect(page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true })).not.toHaveValue('');
  return saved(page);
}
function stableTables(record: any) { return record.politics.run.session.simulations; }
async function observations(page: Page, id: string) {
  return page.evaluate(async id => {
    const db = await new Promise<IDBDatabase>((resolve, reject) => { const r = indexedDB.open('elezioni-result-chunks-v1'); r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); });
    const get = <T>(r: IDBRequest<T>) => new Promise<T>((resolve, reject) => { r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); });
    try {
      const record = await get(db.transaction('results').objectStore('results').get(id));
      const result: unknown[] = [];
      for (const [index, table] of record.result.storage.tables.entries()) {
        if (!['Reform vote shares', 'List seat-vote plot data', 'Reform statistical series v1', 'Diagnostic events v1'].includes(table.name)) continue;
        const chunks = await get(db.transaction('chunks').objectStore('chunks').getAll(IDBKeyRange.bound([id, index, 0], [id, index, Number.MAX_SAFE_INTEGER])));
        const rows = [];
        for (const chunk of chunks.slice(0, table.chunks)) {
          const raw = await new Response(new Blob([chunk.bytes]).stream().pipeThrough(new DecompressionStream('gzip'))).text();
          rows.push(...JSON.parse(raw));
        }
        result.push([table.name, table.columns, rows]);
      }
      return result;
    } finally { db.close(); }
  }, id);
}
for (const [law, model] of [[ROSATELLUM, 'M01'], [REFORM, 'M01'], [REFORM, 'M02']] as const) {
  test(`common lifecycle ${law}/${model}: durable worker runs, queries, reload and archive`, async ({ page, context }) => {
    const f = await setup(context, page, law, model);
    const first = await run(page);
    expect(first.result.completedSimulations).toBe(3); expect(first.politics.run.execution.actualWorkers).toBe(1);
    expect(first.politics.run.executedScenario.execution).toEqual(f.scenario.execution);
    const original = first.politics.run.executedScenario;
    const firstObservations = await observations(page, first.id); expect(firstObservations.length).toBeGreaterThan(0);
    await expect(page.getByRole('alert')).toHaveCount(0);
    await page.getByLabel('Nome scenario', { exact: true }).fill('Edited after execution'); await page.getByLabel('Nome scenario', { exact: true }).blur();
    await expect(page.getByText('Scenario modificato. Risultati ed export conservano lo scenario eseguito.', { exact: true })).toBeVisible();
    const download = page.waitForEvent('download');
    await page.getByRole('button', { name: 'Scarica archivio risultati JSON.gz', exact: true }).click();
    const file = await download, bytes = await readFile((await file.path())!);
    await page.getByLabel('Importa risultati', { exact: true }).setInputFiles({ name: 'common.json.gz', buffer: bytes, mimeType: 'application/gzip' });
    await expect(page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true }).locator('option')).toHaveCount(3);
    expect(await page.evaluate(key => JSON.parse(localStorage.getItem(key)!).name, politicsDraftKey)).toBe('Edited after execution');
    const imported = await saved(page);
    expect(imported.politics.run.executedScenario).toEqual(original);
    expect(stableTables(imported)).toEqual(stableTables(first));
    expect(await observations(page, imported.id)).toEqual(firstObservations);
    await page.reload();
    await expect(page.getByText('Scenario modificato. Risultati ed export conservano lo scenario eseguito.', { exact: true })).toBeVisible();
    await expect(page.getByRole('combobox', { name: 'Tabella dei risultati', exact: true })).toBeVisible();
    if (model === 'M02') await expect(page.locator('.editor-status summary')).toContainText('fitting riutilizzato', { timeout: 60000 });
    await page.getByRole('button', { name: 'Carica lo scenario di questa sessione', exact: true }).click();
    await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue(original.name);
    await expect(page.locator('.editor-status summary')).toContainText('Modello pronto', { timeout: 60000 });
    // Only an explicit field edit changes worker policy. No acceptance run exceeds 3.
    await page.getByLabel('Numero di worker richiesti', { exact: true }).fill('2'); await page.getByLabel('Numero di worker richiesti', { exact: true }).blur();
    const second = await run(page);
    expect(second.politics.run.execution.actualWorkers).toBe(2); expect(stableTables(second)).toEqual(stableTables(first));
    expect(await observations(page, second.id)).toEqual(firstObservations);
    if (model === 'M02') {
      await page.getByText('Fitting e checkpoint', { exact: true }).click();
      const checkpointDownload = page.waitForEvent('download');
      await page.getByRole('button', { name: 'Scarica checkpoint', exact: true }).click();
      const checkpointFile = await checkpointDownload, checkpoint = await readFile((await checkpointFile.path())!);
      await page.getByLabel('Carica checkpoint', { exact: true }).setInputFiles({ name: 'fit.json', buffer: checkpoint, mimeType: 'application/json' });
      await expect(page.locator('.editor-status summary')).toContainText('fitting riutilizzato', { timeout: 60000 });
      expect(await page.evaluate(key => JSON.parse(localStorage.getItem(key)!).execution.simulationSeed, politicsDraftKey)).toBe(original.execution.simulationSeed);
    }
    const broken = new File(['{"kind":"politics-result-archive","schemaVersion":1,"run":'], 'corrupt.json');
    const selected = await page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true }).inputValue();
    await page.getByLabel('Importa risultati', { exact: true }).setInputFiles({ name: broken.name, buffer: Buffer.from(await broken.text()), mimeType: 'application/json' });
    await expect(page.getByRole('alert')).toContainText('Risultati non caricati');
    await expect(page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true })).toHaveValue(selected);
    // Scenario exports still describe the draft, never whichever archive was viewed.
    const scenarioDownload = page.waitForEvent('download');
    await page.getByRole('button', { name: 'Scarica scenario JSON', exact: true }).click();
    const sf = await scenarioDownload;
    const current: PoliticsScenario = decodeScenario(new Uint8Array(await readFile((await sf.path())!)));
    expect(current.execution.workers).toEqual({ kind: 'fixed', count: 2 });
  });
}
test('quota interruption retains only committed simulation membership and the requested denominator', async ({ page, context }) => {
  await context.route('**/politics-workspace.worker-*.js', async route => {
    const response = await route.fetch();
    const sabotage = `let cp07Block=false;const cp07Put=IDBObjectStore.prototype.put;IDBObjectStore.prototype.put=function(v,...a){if(this.name==='results'&&v.politics&&v.result.completedSimulations>=1)cp07Block=true;return cp07Put.call(this,v,...a)};const cp07Tx=IDBDatabase.prototype.transaction;IDBDatabase.prototype.transaction=function(s,m,...a){if(cp07Block&&s==='chunks'&&m==='readwrite')throw new DOMException('CP07 injected storage quota','QuotaExceededError');return cp07Tx.call(this,s,m,...a)};\n`;
    await route.fulfill({ response, body: sabotage + await response.text() });
  });
  await setup(context, page, ROSATELLUM, 'M01');
  const result = await run(page);
  expect(result.result.storage.state).toBe('interrupted'); expect(result.result.completedSimulations).toBe(1);
  expect(result.politics.run.session.simulations).toEqual([{ id: 1, outcome: 'complete' }]);
  expect(result.result.benchmark.simulations).toBe(3);
  expect(result.result.warnings.some((w: any) => w.code === 'RESULT_STORAGE_ERROR')).toBe(true);
  await page.reload(); await expect(page.getByText(/^Scenario eseguito:.*1\/3 simulazioni conservate/)).toBeVisible();
});
test('explicit cancellation locks execution and retains assigned simulation IDs across reload', async ({ page, context }) => {
  await setup(context, page, ROSATELLUM, 'M01');
  await page.getByLabel('Numero di simulazioni', { exact: true }).fill('10'); await page.getByLabel('Numero di simulazioni', { exact: true }).blur();
  await expect(page.locator('.editor-status summary')).toContainText('Modello pronto');
  await page.getByRole('button', { name: 'Avvia simulazione', exact: true }).click();
  await expect(page.getByRole('combobox', { name: 'Algoritmo di scrutinio', exact: true })).toBeDisabled();
  await expect(page.getByRole('button', { name: 'Genera seed casuale delle simulazioni', exact: true })).toBeDisabled();
  await page.getByRole('button', { name: 'Annulla esecuzione', exact: true }).last().click();
  await expect(page.locator('.editor-status summary')).toContainText('Esecuzione interrotta', { timeout: 60000 });
  const result = await saved(page), n = result.result.completedSimulations;
  expect(n).toBeLessThan(10); expect(result.result.benchmark.simulations).toBe(10);
  expect(result.politics.run.session.simulations.map((s: any) => s.id)).toEqual(Array.from({ length: n }, (_, i) => i + 1));
  const selected = result.id;
  await page.reload(); await expect(page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true })).toHaveValue(selected);
});
test('scenario metadata routes regional and common politics files without sharing their drafts', async ({ page, context }) => {
  const f = await setup(context, page, ROSATELLUM, 'M01');
  const regional = { schema_version: 1, scenario: { ...generatedDefaultRegionalErScenario, name: 'Regional handoff' } };
  await page.getByLabel('Carica scenario', { exact: true }).setInputFiles({ name: 'region.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(regional)) });
  await expect(page).toHaveURL(new RegExp(`${base}/emilia-romagna/?$`));
  await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue('Regional handoff');
  await page.getByTestId('regional-scenario-file-input').setInputFiles({ name: 'politics.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify({ ...f.scenario, name: 'Common handoff' })) });
  await expect(page).toHaveURL(new RegExp(`${base}/politics/?$`));
  await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue('Common handoff');
});
