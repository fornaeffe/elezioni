import { expect, test } from '@playwright/test';
import { existsSync } from 'node:fs';
import { resolve } from 'node:path';

const hasRegionalErStaticSnapshot = existsSync(resolve(process.cwd(), 'static/data/v1/regional-er-static.json'));

test.skip(
  !hasRegionalErStaticSnapshot,
  'Generated regional Emilia-Romagna static snapshot is missing. Run scripts/export_regional_er_static_snapshot.R first.'
);

test('runs the Emilia-Romagna regional worker path from the scenario editor', async ({ page }) => {
  await page.goto('/emilia-romagna');

  await expect(page.getByRole('heading', { name: /Emilia-Romagna/ })).toBeVisible();
  await expect(page.getByText('senza generazione di candidati individuali')).toBeVisible();
  await expect(page.getByLabel('Tipo candidato')).toHaveCount(0);
  await expect(page.locator('.candidate-template-group')).toHaveCount(0);

  await page.getByLabel('Simulazioni').fill('1');
  await page.getByRole('button', { name: 'Esegui' }).click();

  await expect(page.getByLabel('Note simulazione')).toContainText('REGIONAL_ER_STATIC_SNAPSHOT', { timeout: 30_000 });
  await expect(page.getByLabel('Avvisi simulazione')).toHaveCount(0);
  await expect(page.getByRole('table', { name: 'Regional election overview' })).toBeVisible();
  await expect(page.getByRole('table', { name: 'Regional coalition outcomes' })).toBeVisible();
  await expect(page.getByRole('table', { name: 'Regional province seats by list' })).toBeVisible();
  await expect(page.getByRole('region', { name: 'Seggi medi per lista' })).toBeVisible();
  await expect(page.getByRole('region', { name: 'Percentuali medie sui voti validi' })).toBeVisible();
  await expect(page.getByRole('button', { name: 'Scarica risultati compressi' })).toBeVisible();
  await expect(page.getByRole('button', { name: 'Scarica risultati CSV' })).toBeVisible();
  await expect(page.getByRole('table', { name: 'Generated regional runs' })).toHaveCount(0);
  await page.getByRole('button', { name: 'Mostra dettagli' }).click();
  await expect(page.getByRole('table', { name: 'Generated regional runs' })).toBeVisible();
});
