import { expect, test } from '@playwright/test';

test('shows the workflow selector on the landing page', async ({ page }) => {
  await page.goto('/');

  await expect(page.getByRole('heading', { name: 'Scegli il flusso di simulazione' })).toBeVisible();
  await expect(page.getByRole('link', { name: /politiche/i })).toHaveAttribute('href', '/politics');
  await expect(page.getByRole('link', { name: /emilia-romagna/i })).toHaveAttribute('href', '/emilia-romagna');
});
