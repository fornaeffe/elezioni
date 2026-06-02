import { expect, test } from '@playwright/test';

test('runs the worker smoke path from the scenario editor', async ({ page }) => {
  await page.goto('/');

  const listRows = page.locator('.list-row');
  for (let index = 0; index < (await listRows.count()); index += 1) {
    const row = listRows.nth(index);
    if ((await row.getByLabel('Nome lista').inputValue()) !== 'Partito Democratico') continue;

    await row.getByLabel('Quota iniziale').fill('30');
    await expect(row.getByLabel('Usa quota Partito Democratico')).toBeChecked();
    break;
  }

  await page.getByRole('button', { name: 'Esegui' }).click();

  await expect(page.getByText('POLITICS_DEBUG_STATIC_SNAPSHOT')).toBeVisible({ timeout: 30_000 });
  const projectionRow = page.getByRole('table', { name: 'Scenario projection' }).locator('tbody tr', {
    hasText: 'Partito Democratico'
  });
  await expect(projectionRow).toContainText('30');
  await expect(projectionRow).toContainText('true');
  await expect(page.getByRole('table', { name: 'Generated pipeline runs' })).toBeVisible();
});
