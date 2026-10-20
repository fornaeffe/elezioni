import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gzipSync, gunzipSync } from 'fflate';
import { expect, test } from '@playwright/test';
import catalog from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json' with { type: 'json' };
const defaults = JSON.parse(readFileSync(new URL(`../../src/lib/politics/reform-2026/data/${catalog.defaultsPath}`, import.meta.url), 'utf8'));

const base = process.env.BASE_PATH ?? '';
const hasBundle = existsSync(resolve(process.cwd(), 'static', catalog.path));

test('both algorithms remain selectable; missing production bundle fails without debug fallback', async ({ page, context }) => {
  await page.goto(`${base}/`);
  await page.getByRole('link', { name: /Politiche/ }).click();
  await expect(page).toHaveURL(new RegExp(`${base}/politics/?$`));
  await expect(page.getByLabel('Algoritmo di scrutinio')).toHaveValue('politiche-r-parity-v1');
  await context.route(`**/${catalog.path}`, route => route.fulfill({ status: 404, body: 'missing' }));
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  await page.getByRole('button', { name: 'Esegui riforma', exact: true }).click();
  await expect(page.getByTestId('reform-phase')).toHaveText('not_implemented', { timeout: 30000 });
  await expect(page.getByLabel('Avvisi riforma')).toContainText('Missing production bundle');
  await expect(page.getByLabel('Avvisi riforma')).toContainText(`${base}/${catalog.path}`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-r-parity-v1');
  await expect(page.getByRole('heading', { name: 'Scenario', exact: true })).toBeVisible();
});

test('production paired worker, bounded detail rendering, scenario/results roundtrip and replay', async ({ page }) => {
  test.skip(!hasBundle, 'Required generated reform runtime bundle is absent; run data:reform:generate -- --publish. Release acceptance must not skip.');
  test.setTimeout(120000);
  const pageErrors: string[] = [];
  page.on('pageerror', error => pageErrors.push(error.message));
  await page.goto(`${base}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  await page.getByLabel('Simulazioni', { exact: true }).fill('2');
  await page.getByRole('button', { name: 'Esegui riforma', exact: true }).click();
  await expect(page.getByTestId('reform-phase')).toHaveText(/completed|not_implemented/, { timeout: 60000 });
  expect(await page.getByTestId('reform-phase').textContent(), await page.locator('body').innerText()).toBe('completed');
  await expect(page.getByRole('table', { name: 'Reform overview' })).toContainText('2');
  await expect(page.getByLabel('Avvisi riforma')).toContainText('R26_CONDITIONAL_CANDIDATES');
  await page.getByLabel('Tabella riforma').selectOption({ label: 'Seat entitlement and election locations (1176)' });
  await expect(page.locator('tbody tr')).toHaveCount(50);
  await expect(page.getByRole('table')).toHaveCount(1);
  await page.getByRole('button', { name: 'Pagina successiva', exact: true }).click();
  await expect(page.getByText('Pagina 2 di 24')).toBeVisible();
  await page.getByLabel('Ramo grafico').selectOption('senato');
  await expect(page.getByRole('figure')).toContainText('senato');

  // Edits after execution must not alter the exported/replay scenario.
  await page.getByLabel('Seed', { exact: true }).fill('edited-after-run');
  await expect(page.getByRole('alert')).toContainText('Scenario modificato');
  const downloadPromise = page.waitForEvent('download');
  await page.getByRole('button', { name: 'Scarica risultati JSON.gz', exact: true }).click();
  const download = await downloadPromise, stream = await download.createReadStream(), chunks: Buffer[] = [];
  for await (const chunk of stream!) chunks.push(Buffer.from(chunk));
  const compressed = Buffer.concat(chunks), payload = JSON.parse(new TextDecoder().decode(gunzipSync(compressed)));
  expect(payload.scenario.seed).toBe(defaults.seed);
  expect(payload.result.benchmark.scrutinyAlgorithmId).toBe('politiche-reform-2026-v1');
  expect(payload.result.tables.find((t: { name: string }) => t.name === 'Reform runs').rows.map((r: unknown[]) => r[0])).toEqual([1, 2]);
  const firstTables = payload.result.tables;
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-r-parity-v1');
  await page.getByTestId('result-file-input').setInputFiles({ name: 'r26-results.json.gz', mimeType: 'application/gzip', buffer: compressed });
  await expect(page.getByLabel('Algoritmo di scrutinio')).toHaveValue('politiche-reform-2026-v1');
  await expect(page.getByTestId('reform-phase')).toHaveText('imported');
  await expect(page.getByLabel('Seed', { exact: true })).toHaveValue(defaults.seed);
  // A second import starts from an already-imported status. Wait for the new
  // session, rather than accepting the previous result's status immediately.
  const previousImportId = await page.getByLabel('Risultati locali').inputValue();
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'r26-results.json.gz', mimeType: 'application/gzip', buffer: compressed });
  await expect(page.getByLabel('Risultati locali')).not.toHaveValue(previousImportId);
  await expect(page.getByTestId('reform-phase')).toHaveText('imported');
  await page.getByRole('button', { name: 'Esegui riforma', exact: true }).click();
  await expect(page.getByTestId('reform-phase')).toHaveText(/completed|not_implemented/, { timeout: 60000 });
  expect(await page.getByTestId('reform-phase').textContent(), await page.locator('body').innerText()).toBe('completed');
  const replayDownload = page.waitForEvent('download');
  await page.getByRole('button', { name: 'Scarica risultati JSON.gz', exact: true }).click();
  const replay = await (await replayDownload).createReadStream(), replayChunks: Buffer[] = [];
  for await (const chunk of replay!) replayChunks.push(Buffer.from(chunk));
  expect(JSON.parse(new TextDecoder().decode(gunzipSync(Buffer.concat(replayChunks)))).result.tables).toEqual(firstTables);
  await page.screenshot({ path: 'test-results/reform-workspace.png', fullPage: true });
  expect(pageErrors).toEqual([]);
  // Plain new scenario import retains identity, never goes through legacy parsing.
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'r26-scenario.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(defaults)) });
  await expect(page.getByLabel('Seed', { exact: true })).toHaveValue(defaults.seed);
  // Legacy result import in the reform workspace explicitly restores its law.
  const legacy = { schema_version: 2, exportedAt: '2026-10-20T00:00:00Z', scenario: {
    id: 'old', name: 'Old saved scenario', electionDate: '2027-09-25', defaultSource: { kind: 'manual', electionKind: 'politiche', territory: 'Italia', dataVersion: 'v1' },
    globalShareMode: 'mean', abstentionShare: 30, abstentionOverride: false, lists: [], coalitions: [], listCorrespondences: [], localShareOverrides: [], candidateTemplates: [],
    candidateGeneration: { uninominalToPlurinominalShare: 0, plurinominalCandidacyCountShares: [1, 0, 0, 0, 0] }
  }, result: { type: 'result', status: 'completed', tables: [], warnings: [], benchmark: { startedAt: '2026-10-20T00:00:00Z', elapsedMs: 0, simulations: 1, dataVersion: 'v1' } } };
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'legacy.json.gz', mimeType: 'application/gzip', buffer: Buffer.from(gzipSync(new TextEncoder().encode(JSON.stringify(legacy)))) });
  await expect(page.getByLabel('Algoritmo di scrutinio')).toHaveValue('politiche-r-parity-v1');
});
