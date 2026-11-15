import { expect, test } from '@playwright/test';
import authored from '../../../data/politics-default-scenario.json' with { type: 'json' };
import { base } from '../fixtures/common-page';
test('both politics entry routes share a durable draft; regional navigation remains independent', async ({ page }) => {
    await page.goto(base + '/politics');
    const name = page.getByLabel('Nome scenario', { exact: true });
    await expect(name).toHaveValue(authored.name);
    await name.fill('One shared draft');
    await name.blur();
    await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-r-parity-v1');
    await expect(name).toHaveValue('One shared draft');
    await page.goto(base + '/politics/editor');
    await expect(name).toHaveValue('One shared draft');
    await page.goto(base + '/');
    await page.getByRole('link', { name: /emilia-romagna/i }).click();
    await expect(page.locator('[data-workspace=regional-er]')).toBeVisible();
    await page.goto(base + '/politics');
    await expect(name).toHaveValue('One shared draft');
});
