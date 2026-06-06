import { expect, test } from '@playwright/test';
import { existsSync } from 'node:fs';
import { resolve } from 'node:path';

const hasPoliticsStaticSnapshot =
  existsSync(resolve(process.cwd(), 'static/data/v1/politics-static.json')) ||
  existsSync(resolve(process.cwd(), 'static/data/v1/politics-static-debug.json'));

test.skip(!hasPoliticsStaticSnapshot, 'Generated politics static snapshot is missing. Run scripts/export_politics_static_snapshot.R first.');

test('runs the worker smoke path from the scenario editor', async ({ page }) => {
  await page.goto('/politics');

  await page.getByRole('button', { name: 'Impostazioni avanzate' }).click();
  await page.getByLabel('Astensione elettori').fill('50');
  await expect(page.getByLabel('Usa astensione')).toBeChecked();

  const listRows = page.locator('.list-row');
  for (let index = 0; index < (await listRows.count()); index += 1) {
    const row = listRows.nth(index);
    if ((await row.getByLabel('Nome lista').inputValue()) !== '+Europa') continue;

    await row.getByLabel('Nome lista').fill('+Europa Test');
    break;
  }

  const unitedStatesMapping = page.locator('.correspondence-source', { hasText: "STATI UNITI D'EUROPA" });
  await expect(unitedStatesMapping.getByLabel('Destinazione corrispondenza').first()).toHaveValue('+Europa Test');
  await expect(unitedStatesMapping.getByLabel('Fattore corrispondenza').first()).toHaveValue('0.5');

  for (let index = 0; index < (await listRows.count()); index += 1) {
    const row = listRows.nth(index);
    if ((await row.getByLabel('Nome lista').inputValue()) !== 'Partito Democratico') continue;

    await row.getByLabel('Quota iniziale').fill('30');
    await expect(row.getByLabel('Usa quota Partito Democratico')).toBeChecked();
    break;
  }

  await page.getByLabel('Cerca localita').fill('Aglie');
  await page.getByRole('listbox', { name: 'Localita trovate' }).getByRole('button', { name: /001001/ }).click();
  await page.getByLabel('Lista quota locale', { exact: true }).selectOption('Partito Democratico');
  await page.getByLabel('Quota locale da aggiungere').fill('42');
  await page.getByRole('button', { name: 'Aggiungi quota locale' }).click();

  let localOverrideGroup = page.locator('.local-override-group', { hasText: 'Agliè' });
  await expect(localOverrideGroup).toContainText('42.0%');
  await localOverrideGroup.getByRole('spinbutton', { name: 'Quota locale Partito Democratico' }).fill('40');
  await expect(localOverrideGroup).toContainText('40.0%');
  await localOverrideGroup.getByRole('button', { name: 'Rimuovi quota locale Partito Democratico' }).click();
  await expect(page.locator('.local-override-group', { hasText: 'Agliè' })).toHaveCount(0);

  await page.getByRole('button', { name: 'Aggiungi quota locale' }).click();
  localOverrideGroup = page.locator('.local-override-group', { hasText: 'Agliè' });
  await expect(localOverrideGroup).toContainText('42.0%');

  await page.getByLabel('Ambito quota locale').selectOption('province');
  await page.getByLabel('Cerca localita').fill('Roma');
  await page.getByRole('listbox', { name: 'Localita trovate' }).getByRole('button', { name: /^Roma \(/ }).click();
  await page.getByLabel('Lista quota locale', { exact: true }).selectOption('Partito Democratico');
  await page.getByLabel('Quota locale da aggiungere').fill('25');
  await page.getByRole('button', { name: 'Aggiungi quota locale' }).click();
  await expect(page.locator('.local-override-group', { hasText: 'Roma' })).toContainText('25.0%');

  await page.getByLabel('Ambito quota locale').selectOption('region');
  await page.getByLabel('Cerca localita').fill('Lombardia');
  await page.getByRole('listbox', { name: 'Localita trovate' }).getByRole('button', { name: /Lombardia/ }).click();
  await page.getByLabel('Lista quota locale', { exact: true }).selectOption('Partito Democratico');
  await page.getByLabel('Quota locale da aggiungere').fill('20');
  await page.getByRole('button', { name: 'Aggiungi quota locale' }).click();
  await expect(page.locator('.local-override-group', { hasText: 'Lombardia' })).toContainText('20.0%');

  await page.getByLabel('Coalizione candidato').selectOption('sinistra');
  await page.getByLabel('Cerca collegio candidato').fill('Valle');
  await page.getByRole('listbox', { name: 'Collegi candidati trovati' }).getByRole('button', { name: /Valle/ }).click();
  await page.getByLabel('Nome candidato').fill('Candidata Uni Test');
  await page.getByLabel('Data nascita candidato').fill('1980-01-02');
  await page.getByRole('button', { name: 'Aggiungi candidato' }).click();
  await expect(page.locator('.candidate-template-group', { hasText: 'Camera uninominale' })).toBeVisible();
  await expect(page.getByLabel('Nome Candidata Uni Test')).toHaveValue('Candidata Uni Test');

  await page.getByLabel('Tipo candidato', { exact: true }).selectOption('plurinominal');
  await page.getByLabel('Lista candidato', { exact: true }).selectOption('Partito Democratico');
  await page.getByLabel('Cerca collegio candidato').fill('Piemonte 1 P01');
  await page.getByRole('listbox', { name: 'Collegi candidati trovati' }).getByRole('button', { name: /Piemonte 1 - P01/ }).click();
  await page.getByLabel('Numero candidato').selectOption('1');
  await page.getByLabel('Nome candidato').fill('Candidata Pluri Test');
  await page.getByLabel('Data nascita candidato').fill('1985-03-04');
  await page.getByRole('button', { name: 'Aggiungi candidato' }).click();
  await expect(page.locator('.candidate-template-group', { hasText: 'Camera plurinominale' })).toBeVisible();
  await expect(page.getByLabel('Nome Candidata Pluri Test')).toHaveValue('Candidata Pluri Test');

  await page.getByRole('button', { name: 'Esegui' }).click();

  await expect(page.getByLabel('Note simulazione')).toContainText('POLITICS_STATIC_SNAPSHOT', { timeout: 30_000 });
  await expect(page.getByLabel('Avvisi simulazione')).toHaveCount(0);
  const projectionRow = page.getByRole('table', { name: 'Scenario projection' }).locator('tbody tr', {
    hasText: 'Partito Democratico'
  });
  await expect(projectionRow).toContainText('30');
  await expect(projectionRow).toContainText('true');
  const renamedProjectionRow = page.getByRole('table', { name: 'Scenario projection' }).locator('tbody tr', {
    hasText: '+Europa Test'
  });
  await expect(renamedProjectionRow).toContainText('historical');
  await expect(page.getByRole('table', { name: 'Election overview' })).toBeVisible();
  await expect(page.getByRole('region', { name: 'Seggi plurinominali medi' })).toBeVisible();
  await expect(page.getByRole('region', { name: 'Percentuali medie sui voti validi' })).toBeVisible();
  await page.getByText('Spinogrammi degli eletti').click();
  await page.getByRole('button', { name: /camera - / }).first().click();
  await expect(page.getByRole('dialog')).toContainText('Spinogrammi degli eletti');
  await page.getByRole('dialog').getByRole('button', { name: 'Chiudi' }).click();
  await expect(page.getByRole('dialog')).toHaveCount(0);
  await expect(page.getByRole('table', { name: 'Average plurinominal seats by list' })).toBeVisible();
  await expect(page.getByRole('table', { name: 'Vote share by list' })).toBeVisible();
  await expect(page.getByRole('button', { name: 'Scarica risultati compressi' })).toBeVisible();
  await expect(page.getByRole('button', { name: 'Scarica risultati CSV' })).toBeVisible();
  await expect(page.getByRole('table', { name: 'Generated pipeline runs' })).toHaveCount(0);
  await page.getByRole('button', { name: 'Mostra dettagli' }).click();
  await expect(page.getByRole('table', { name: 'Generated pipeline runs' })).toBeVisible();
});
