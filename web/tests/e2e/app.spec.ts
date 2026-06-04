import { expect, test } from '@playwright/test';
import { existsSync } from 'node:fs';
import { resolve } from 'node:path';

const hasPoliticsStaticSnapshot =
  existsSync(resolve(process.cwd(), 'static/data/v1/politics-static.json')) ||
  existsSync(resolve(process.cwd(), 'static/data/v1/politics-static-debug.json'));

test.skip(!hasPoliticsStaticSnapshot, 'Generated politics static snapshot is missing. Run scripts/export_politics_static_snapshot.R first.');

test('runs the worker smoke path from the scenario editor', async ({ page }) => {
  await page.goto('/');

  await page.getByRole('button', { name: 'Impostazioni avanzate' }).click();
  await page.getByRole('radio', { name: 'Fissa' }).click();
  await expect(page.getByRole('radio', { name: 'Fissa' })).toHaveAttribute('aria-checked', 'true');

  const listRows = page.locator('.list-row');
  for (let index = 0; index < (await listRows.count()); index += 1) {
    const row = listRows.nth(index);
    if ((await row.getByLabel('Nome lista').inputValue()) !== '+Europa') continue;

    await row.getByLabel('Nome lista').fill('+Europa Test');
    break;
  }

  await page.getByRole('button', { name: 'Aggiungi corrispondenza' }).click();
  const correspondenceRow = page.locator('.correspondence-row').first();
  await expect(correspondenceRow.getByLabel('Lista scenario corrispondenza')).toHaveValue('+Europa Test');
  await expect(correspondenceRow.getByLabel('Lista modello corrispondenza')).toHaveValue('+Europa');

  for (let index = 0; index < (await listRows.count()); index += 1) {
    const row = listRows.nth(index);
    if ((await row.getByLabel('Nome lista').inputValue()) !== 'Partito Democratico') continue;

    await row.getByLabel('Quota iniziale').fill('30');
    await expect(row.getByLabel('Usa quota Partito Democratico')).toBeChecked();
    break;
  }

  await page.getByRole('button', { name: 'Esegui' }).click();

  await expect(page.getByText('POLITICS_STATIC_SNAPSHOT')).toBeVisible({ timeout: 30_000 });
  const projectionRow = page.getByRole('table', { name: 'Scenario projection' }).locator('tbody tr', {
    hasText: 'Partito Democratico'
  });
  await expect(projectionRow).toContainText('30');
  await expect(projectionRow).toContainText('true');
  const renamedProjectionRow = page.getByRole('table', { name: 'Scenario projection' }).locator('tbody tr', {
    hasText: '+Europa Test'
  });
  await expect(renamedProjectionRow).toContainText('+Europa');
  await expect(renamedProjectionRow).toContainText('declared-correspondence');
  await expect(page.getByRole('table', { name: 'Average plurinominal seats by list' })).toBeVisible();
  await expect(page.getByRole('table', { name: 'Generated pipeline runs' })).toHaveCount(0);
  await page.getByRole('button', { name: 'Mostra dettagli' }).click();
  await expect(page.getByRole('table', { name: 'Generated pipeline runs' })).toBeVisible();
});
