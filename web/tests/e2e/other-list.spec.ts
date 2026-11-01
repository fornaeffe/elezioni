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

test('reform: Other has explicit component membership and fixed presentation', async ({page})=>{
  await page.goto(`${process.env.BASE_PATH??''}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  const group=page.getByRole('group',{name:'Altre liste',exact:true});
  const disable=group.getByRole('button',{name:'Disattiva Altre liste',exact:true});
  if(await disable.count()) await disable.click();
  await group.getByRole('button',{name:'Attiva Altre liste',exact:true}).click();
  await expect(page.getByRole('alert')).toContainText('Scegli esplicitamente');
  const components=group.getByLabel('Componenti del residuo');
  const id=await components.locator('option').first().getAttribute('value');
  await components.selectOption(id!);
  await group.getByRole('button',{name:'Attiva Altre liste',exact:true}).click();
  await expect(disable).toBeVisible();
  const other=page.locator('details').filter({has:page.locator(':scope > summary',{hasText:/^Altre liste ·/})});
  await other.locator(':scope > summary').click();
  await expect(other.getByLabel('Nome lista',{exact:true})).toBeDisabled();
  await expect(other.getByLabel('Colore',{exact:true})).toBeDisabled();
  await expect(other.getByLabel('Coalizione',{exact:true})).toHaveCount(0);
  await disable.click();
  await expect(other).toHaveCount(0);
});
