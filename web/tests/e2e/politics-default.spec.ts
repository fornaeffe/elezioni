import { expect, test } from '@playwright/test';
import authored from '../../../data/politics-default-scenario.json' with { type: 'json' };
const base=process.env.BASE_PATH??'';
test.setTimeout(90_000);

test('authored politics startup and reset retain the approved default choices',async({page})=>{
  await page.goto(`${base}/politics`);
  await page.getByText('Impostazioni avanzate della simulazione', { exact: true }).click();
  await expect(page.getByLabel('Legge elettorale')).toHaveValue(authored.selection.scrutiny.id);
  const name=page.getByLabel('Nome scenario',{exact:true});
  await expect(name).toHaveValue(authored.name);
  await expect(page.getByLabel('Seed delle simulazioni',{exact:true})).toHaveValue(authored.execution.simulationSeed);
  await expect(page.getByLabel('Numero di simulazioni',{exact:true})).toHaveValue('10');
  await expect(page.getByLabel("Colore Fratelli d'Italia",{exact:true})).toHaveValue('#003366');
  await page.getByText('Impostazioni avanzate dello scenario',{exact:true}).click();
  await expect(page.getByLabel('Generatore dei voti',{exact:true})).toHaveValue('M02');
  await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]')).toContainText('Modello pronto',{timeout:60000});
  await name.fill('Changed draft');
  await page.getByLabel('Numero di simulazioni',{exact:true}).fill('30');
  await page.getByLabel('Seed delle simulazioni',{exact:true}).fill('changed-seed');
  page.once('dialog',dialog=>dialog.accept());
  await page.getByRole('button',{name:'Ripristina scenario predefinito',exact:true}).click();
  await expect(name).toHaveValue(authored.name);
  await expect(page.getByLabel('Seed delle simulazioni',{exact:true})).toHaveValue(authored.execution.simulationSeed);
  await expect(page.getByLabel('Numero di simulazioni',{exact:true})).toHaveValue('10');
  await expect(page.getByLabel('Generatore dei voti',{exact:true})).toHaveValue('M02');
  await page.reload();
  await expect(name).toHaveValue(authored.name);
});
