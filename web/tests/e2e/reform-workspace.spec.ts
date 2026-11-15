import {currentBrowserSession} from '../fixtures/current-browser-session';
import { expect, test } from '@playwright/test';
import { readFileSync } from 'node:fs';
import catalog from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json' with {type:'json'};
const obsolete = JSON.parse(readFileSync(new URL(`../../src/lib/politics/reform-2026/data/${catalog.defaultsPath}`,import.meta.url),'utf8'));
const base=process.env.BASE_PATH??'';
for (const version of [1,2,3,4]) test(`retired reform scenario v${version} is rejected without opening another workflow`,async({page})=>{
  await page.goto(base+'/politics');
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  await page.getByLabel('Carica scenario', {exact:true}).setInputFiles({name:`v${version}.json`,mimeType:'application/json',buffer:Buffer.from(JSON.stringify({...obsolete,schemaVersion:version}))});
  await expect(page.getByRole('alert')).toContainText(/precedente|version|schemaVersion/i);
  await page.getByText('Impostazioni avanzate dello scenario',{exact:true}).click();
  await expect(page.getByLabel('Generatore dei voti',{exact:true})).toHaveValue('M02');

});

test('former v5 plain scenario is rejected on the previous-law screen',async({page})=>{
  const scenario=structuredClone((await currentBrowserSession()).scenario);
  scenario.seed='current-plain-import';
  await page.goto(base+'/politics');
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-r-parity-v1');
  await page.getByLabel('Carica scenario', {exact:true}).setInputFiles({name:'current.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(scenario))});
  await expect(page.getByLabel('Algoritmo di scrutinio')).toHaveValue('politiche-r-parity-v1');
  await expect(page.getByRole('alert')).toContainText('precedente non supportato');
});
