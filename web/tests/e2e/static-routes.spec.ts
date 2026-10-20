import { expect, test } from '@playwright/test';

test('static pages support direct loads, refreshes and workflow navigation', async ({ page }) => {
  const base = process.env.BASE_PATH ?? '';
  for (const route of ['/', '/politics', '/emilia-romagna']) {
    const response = await page.goto(`${base}${route}`);
    expect(response?.status()).toBe(200);
    await expect(page.locator('h1')).toBeVisible();
    expect((await page.reload())?.status()).toBe(200);
    await expect(page.locator('h1')).toBeVisible();
  }
  await page.goto(`${base}/`);
  await page.getByRole('link', { name: /politiche/i }).click();
  await expect(page.getByLabel('Algoritmo di scrutinio')).toBeVisible();
  await page.goBack();
  await page.getByRole('link', { name: /emilia-romagna/i }).click();
  await expect(page.getByRole('heading', { name: /Emilia-Romagna/ })).toBeVisible();
});
