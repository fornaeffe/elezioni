import { generatedSnapshotPoliticsScenario } from '../fixtures/legacy-snapshot-scenario';
import { expect, test } from '@playwright/test';
import { generatedDefaultPoliticsScenario } from '../../src/lib/scenario/politics-defaults.generated';
import { generatedDefaultRegionalErScenario } from '../../src/lib/scenario/regional-er-defaults.generated';

const base = process.env.BASE_PATH ?? '';
test.setTimeout(90_000);

test('fresh politics default, filing editor, results and reset share one lifecycle', async ({ page }) => {
  await page.goto(`${base}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-r-parity-v1');
  await expect(page.getByLabel('Algoritmo di scrutinio')).toHaveValue('politiche-r-parity-v1');
  await expect(page.locator('.list-row')).toHaveCount(24);
  await expect(page.locator('.coalition-row')).toHaveCount(2);
  await expect(page.getByLabel('Spazi di simulazione')).toBeVisible();
  await page.getByRole('button', { name: 'Impostazioni avanzate' }).click();
  await page.getByText('Presentazione territoriale e minoranze', { exact: true }).click();
  await page.locator('summary').filter({ hasText: /^Ora!$/ }).click();
  await expect(page.getByRole('listbox', { name: 'Camera — Ora!', exact: true }).locator('option:checked')).toHaveCount(14);
  await expect(page.getByRole('listbox', { name: 'Senato — Ora!', exact: true }).locator('option:checked')).toHaveCount(9);
  await page.getByLabel('Simulazioni', { exact: true }).fill('1');
  await page.getByRole('button', { name: 'Esegui', exact: true }).click();
  await expect(page.getByRole('table', { name: 'Election overview', exact: true })).toBeVisible({ timeout: 60_000 });
  await expect(page.getByLabel('Errori simulazione')).toHaveCount(0);
  const name = page.getByLabel('Nome scenario', { exact: true });
  await name.fill('Bozza modificata');
  await expect(page.getByTestId('stale-result-warning')).toBeVisible();
  page.once('dialog', dialog => dialog.accept());
  await page.getByRole('button', { name: 'Ripristina scenario', exact: true }).click();
  await expect(name).toHaveValue(generatedDefaultPoliticsScenario.name);
  await expect(page.getByRole('table', { name: 'Election overview', exact: true })).toBeVisible();
  await expect(page.getByLabel('Avvisi simulazione')).toContainText('POLITICS_SCENARIO_LOCAL_OVERRIDES_OVERLAP');
  await page.screenshot({ path: 'test-results/workspace-politics.png' });
});

test('historical drafts remain historical and cancel cleanly on navigation', async ({ page }) => {
  await page.addInitScript(scenario => localStorage.setItem('elezioni:web:politics-scenario:v1', JSON.stringify({ schema_version: 7, scenario })), generatedSnapshotPoliticsScenario);
  await page.goto(`${base}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-r-parity-v1');
  await expect(page.locator('.list-row')).toHaveCount(9);
  await page.getByLabel('Simulazioni', { exact: true }).fill('1000');
  await page.getByRole('button', { name: 'Esegui', exact: true }).click();
  await expect(page.getByRole('button', { name: 'Annulla calcolo' })).toBeVisible();
  await page.getByRole('button', { name: 'Annulla calcolo' }).click();
  await expect(page.getByRole('button', { name: 'Esegui', exact: true })).toBeEnabled();
  await page.getByRole('button', { name: 'Esegui', exact: true }).click();
  await page.getByLabel('Spazi di simulazione').getByRole('link', { name: 'Regionali Emilia-Romagna', exact: true }).click();
  await expect(page.locator('[data-workspace=regional-er]')).toBeVisible();
  await page.reload();
  await expect(page.getByRole('heading', { name: /Emilia-Romagna/ })).toBeVisible();
});

test('scenario metadata hands imports to the correct route without rewriting them', async ({ page }) => {
  await page.goto(`${base}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-r-parity-v1');
  await page.getByTestId('scenario-file-input').setInputFiles({ name: 'regional.json', mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify({ schema_version: 1, scenario: { ...generatedDefaultRegionalErScenario, name: 'Importazione regionale' } })) });
  await expect(page).toHaveURL(new RegExp(`${base}/emilia-romagna/?$`));
  await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue('Importazione regionale');
  await page.getByTestId('regional-scenario-file-input').setInputFiles({ name: 'politics.json', mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify({ schema_version: 9, scenario: { ...generatedDefaultPoliticsScenario, name: 'Importazione politica' } })) });
  await expect(page).toHaveURL(new RegExp(`${base}/politics/?$`));
  await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue('Importazione politica');
});

test('corrupt required snapshots fail visibly, with no replacement data', async ({ page }) => {
  await page.route('**/data/v1/regional-er-static.json', route => route.fulfill({ status: 200, contentType: 'application/json', body: '{}' }));
  await page.goto(`${base}/emilia-romagna`);
  await page.getByLabel('Simulazioni', { exact: true }).fill('1');
  await page.getByRole('button', { name: 'Esegui', exact: true }).click();
  await expect(page.getByLabel('Errori simulazione')).toContainText('checksum', { timeout: 30_000 });
  await expect(page.getByRole('table', { name: 'Regional election overview', exact: true })).toHaveCount(0);
});
