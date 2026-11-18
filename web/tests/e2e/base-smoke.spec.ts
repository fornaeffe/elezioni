import { expect, test } from '@playwright/test';
import { openConstructedPolitics } from '../fixtures/constructed-page';
import { ROSATELLUM } from '../../src/lib/politics/scenario/schema';

test('root routes refresh navigation and constructed worker execution', async ({ page, context }) => {
  test.setTimeout(90_000);
  const base = process.env.BASE_PATH ?? '';
  const errors: string[] = [];
  page.on('pageerror', e => errors.push(e.message));
  await openConstructedPolitics(context, page, ROSATELLUM, 'M01');
  await page.getByRole('button', { name: 'Avvia simulazione', exact: true }).click();
  await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]'))
    .toContainText('Esecuzione completata', { timeout: 60_000 });
  for (const route of ['/', '/politics/', '/politics/editor/', '/emilia-romagna/']) {
    expect((await page.goto(base + route))?.status()).toBe(200);
    await expect(page.locator('h1')).toBeVisible();
    expect((await page.reload())?.status()).toBe(200);
  }
  await page.goto(base + '/');
  await page.getByRole('link', { name: /politiche/i }).click();
  await expect(page.getByLabel('Legge elettorale')).toBeVisible();
  await page.goto(base + '/');
  await page.getByRole('link', { name: /emilia-romagna/i }).click();
  await expect(page.locator('[data-workspace=regional-er]')).toBeVisible();
  expect(errors).toEqual([]);
});
