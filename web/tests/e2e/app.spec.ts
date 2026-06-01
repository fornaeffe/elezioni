import { expect, test } from '@playwright/test';

test('runs the worker smoke path from the scenario editor', async ({ page }) => {
  await page.goto('/');
  await page.getByRole('button', { name: 'Esegui' }).click();

  await expect(page.getByText('POLITICS_SCRUTINY_NOT_PORTED')).toBeVisible();
  await expect(page.getByRole('table')).toBeVisible();
});
