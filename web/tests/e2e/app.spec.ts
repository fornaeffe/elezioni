import { expect, test } from '@playwright/test';

test('runs the worker smoke path from the scenario editor', async ({ page }) => {
  await page.goto('/');
  await page.getByRole('button', { name: 'Esegui' }).click();

  await expect(page.getByText('POLITICS_SCENARIO_GENERATOR_PENDING')).toBeVisible();
  await expect(page.getByRole('table', { name: 'Direct scrutiny runs' })).toBeVisible();
});
