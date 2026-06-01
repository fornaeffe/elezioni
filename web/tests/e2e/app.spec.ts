import { expect, test } from '@playwright/test';

test('runs the worker smoke path from the scenario editor', async ({ page }) => {
  await page.goto('/');
  await page.getByRole('button', { name: 'Esegui' }).click();

  await expect(page.getByText('POLITICS_DEBUG_PIPELINE_SOURCE')).toBeVisible({ timeout: 30_000 });
  await expect(page.getByRole('table', { name: 'Generated pipeline runs' })).toBeVisible();
});
