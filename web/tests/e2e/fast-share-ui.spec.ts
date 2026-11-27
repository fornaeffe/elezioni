import { expect, test } from '@playwright/test';
import { installEditorPreparation } from '../fixtures/editor-preparation';
import { waitForPoliticsReady } from '../fixtures/politics-readiness';
import { politicsDraftKey } from '../../src/lib/politics/files/draft';

test.setTimeout(60_000);

test('explicit refit cancellation and retry do not overwrite the draft', async ({ page }) => {
    await installEditorPreparation(page, true);
    await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
    await waitForPoliticsReady(page);
    await page.getByLabel('Nome scenario', { exact: true }).fill('Draft retained during cancellation');
    await page.getByLabel('Nome scenario', { exact: true }).blur();
    await waitForPoliticsReady(page);
    const original = await page.evaluate(key => JSON.parse(localStorage.getItem(key) ?? 'null'), politicsDraftKey);
    await page.getByText('Impostazioni avanzate della simulazione', { exact: true }).click();
    await page.getByText('Fitting e checkpoint', { exact: true }).click();
    await page.getByRole('button', { name: 'Ricalcola fitting', exact: true }).click();
  const status = page.getByRole('complementary', { name: "Stato dell'applicazione" });
  await status.getByRole('button', { name: 'Stato: Preparazione del modello' }).click();
  await expect(status.getByText('Controlled editor refit · 0/1', { exact: true })).toBeVisible();
  await status.getByRole('button', { name: 'Annulla', exact: true }).click();
  await expect(status).toContainText('Preparazione annullata');
    await page.getByRole('button', { name: 'Ricalcola fitting', exact: true }).click();
    await waitForPoliticsReady(page);
    const stored = await page.evaluate(key => JSON.parse(localStorage.getItem(key) ?? 'null'), politicsDraftKey);
    expect(stored).toEqual(original);
});
