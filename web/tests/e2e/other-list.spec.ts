import { expect, test } from '@playwright/test';
test.setTimeout(90_000);

for (const route of ['politics', 'emilia-romagna']) {
  test(`${route}: unique fixed Other switch, reference share and persistence`, async ({ page }) => {
    await page.goto(`${process.env.BASE_PATH ?? ''}/${route}`);
    const toggle = page.getByRole('switch', { name: 'Includi Altre liste' });
    await expect(toggle).not.toBeChecked();
    await toggle.check();
    const row = page.getByTestId('other-list-row');
    await expect(row).toHaveCount(1);
    await expect(row.locator('input[type=color], input[type=text], select')).toHaveCount(0);
    await row.screenshot({ path: `test-results/other-list-${route}.png` });
    await page.setViewportSize({ width: 390, height: 844 });
    expect(await row.evaluate(e => e.scrollWidth)).toBeLessThanOrEqual(await row.evaluate(e => e.clientWidth));
    await row.screenshot({ path: `test-results/other-list-${route}-mobile.png` });
    await row.getByLabel('Quota Altre liste', { exact: true }).fill('10');
    await expect(row.getByLabel('Usa quota Altre liste')).toBeChecked();
    await page.reload();
    await expect(toggle).toBeChecked();
    await expect(row.getByLabel('Quota Altre liste', { exact: true })).toHaveValue('10');
    await page.getByLabel('Simulazioni', { exact: true }).fill('1');
    await page.getByRole('button', { name: 'Esegui', exact: true }).click();
    const result = page.getByRole('table', { name: route === 'politics' ? 'Average plurinominal seats by list' : 'Regional average seats by list', exact: true });
    await expect(result).toBeVisible({ timeout: 60_000 });
    for (const r of await result.getByRole('row').filter({ hasText: 'Altre liste' }).all()) await expect(r).toContainText('0');
    await toggle.uncheck();
    await expect(row).toHaveCount(0);
    await toggle.check();
    await expect(row).toHaveCount(1);
    await expect(row.getByLabel('Usa quota Altre liste')).not.toBeChecked();
  });
}

test('reform: Other keeps reference/history/coverage editors without candidate controls', async ({ page }) => {
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  const toggle = page.getByRole('switch', { name: 'Includi Altre liste' });
  await toggle.check();
  const row = page.getByTestId('other-list-row');
  await expect(row).toHaveCount(1);
  await expect(row.locator('input[type=color], input[type=text], select')).toHaveCount(0);
  await page.getByLabel('Quota Altre liste', { exact: true }).fill('10');
  await page.getByLabel('Quota Altre liste', { exact: true }).press('Tab');
  await page.reload();
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  await expect(toggle).toBeChecked();
  await expect(page.getByLabel('Quota Altre liste', { exact: true })).toHaveValue('10');
  await page.getByText('Depositi delle liste', { exact: true }).click();
  await page.getByLabel('Lista deposito').selectOption('__other__');
  await expect(page.getByLabel('Lista deposito')).toHaveValue('__other__');
  await page.getByLabel('Simulazioni', { exact: true }).fill('1');
  await page.getByRole('button', { name: 'Esegui riforma', exact: true }).click();
  await expect(page.getByRole('button', { name: 'Scarica risultati JSON.gz', exact: true })).toBeEnabled({ timeout: 60_000 });
  await toggle.uncheck();
  await page.getByRole('button', { name: 'Applica modifica e rimuovi dipendenze' }).click();
  await expect(row).toHaveCount(0);
});
