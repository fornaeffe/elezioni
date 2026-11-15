import { expect, test } from '@playwright/test';
import authored from '../../../data/politics-default-scenario.json' with { type: 'json' };

test('ignores and leaves both former politics draft keys untouched', async ({ page }) => {
  const keys = ['elezioni:web:politics-scenario:v1', 'elezioni:politics-reform-2026:fast-draft:v1'];
  await page.addInitScript(keys => { for (const key of keys) localStorage.setItem(key, '{obsolete corrupt draft}'); }, keys);
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue(authored.name);
  await expect(page.getByRole('alert')).toHaveCount(0);
  await page.getByLabel('Nome scenario', { exact: true }).fill('Temporary edit');
  await page.getByRole('combobox', { name: 'Legge elettorale', exact: true }).selectOption('politiche-r-parity-v1');
  await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue('Temporary edit');
  await page.getByLabel('Nome scenario', { exact: true }).fill('Another temporary edit');
  expect(await page.evaluate(keys => keys.map(key => localStorage.getItem(key)), keys)).toEqual(keys.map(() => '{obsolete corrupt draft}'));
  await page.getByLabel('Nome scenario', { exact: true }).blur();
  await page.reload();
  await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue('Another temporary edit');
  await expect(page.getByRole('alert')).toHaveCount(0);
});

test('malformed common imports cannot switch the law or replace the current draft', async ({page})=>{
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  const name=page.getByLabel('Nome scenario',{exact:true});
  await name.fill('Current common draft'); await name.blur();
  const otherLaw={...authored, selection:{...authored.selection,scrutiny:{id:'politiche-r-parity-v1',version:'1.0.0'}}, unexpected:true};
  await page.getByLabel('Carica scenario',{exact:true}).setInputFiles({name:'invalid.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(otherLaw))});
  await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]')).toContainText('Scenario non caricato');
  await expect(name).toHaveValue('Current common draft');
  await expect(page.getByRole('combobox', { name: 'Legge elettorale', exact: true })).toHaveValue(authored.selection.scrutiny.id);
});
