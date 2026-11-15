import { expect, test } from '@playwright/test';

test('status remains fixed and compact on mobile, with accessible error details', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 700 });
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  const box = page.locator('aside[aria-label="Stato dell\'applicazione"]');
  const toggle = page.getByRole('button', { name: /^Stato:/ });
  await expect(toggle).toBeVisible();
  await expect(toggle.locator(':scope > span').last()).toBeHidden();
  const before = await box.boundingBox();
  await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
  const after = await box.boundingBox();
  expect(Math.abs(before!.y - after!.y)).toBeLessThan(1);
  await page.getByLabel('Carica scenario', { exact: true }).setInputFiles({ name: 'bad.json', mimeType: 'application/json', buffer: Buffer.from('{') });
  await expect(box).toHaveClass(/error/);
  await toggle.click();
  await expect(box.getByText(/Scenario non caricato/)).toBeVisible();
  await box.getByRole('button', { name: 'Chiudi avviso' }).click();
  await expect(box).not.toHaveClass(/error/);
});

test('regional import errors appear in the shared status box', async ({ page }) => {
  await page.goto(`${process.env.BASE_PATH ?? ''}/emilia-romagna`);
  const box = page.locator('aside[aria-label="Stato dell\'applicazione"]');
  await page.locator('input[type=file]').first().setInputFiles({ name: 'bad.json', mimeType: 'application/json', buffer: Buffer.from('{') });
  await expect(box).toHaveClass(/error/);
  await page.getByRole('button', { name: /^Stato:/ }).click();
  await expect(box.locator('strong').filter({ hasText: 'Operazione regionale non riuscita' })).toBeVisible();
});

