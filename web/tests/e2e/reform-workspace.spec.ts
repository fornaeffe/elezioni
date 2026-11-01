import {currentBrowserSession} from '../fixtures/current-browser-session';
import { expect, test } from '@playwright/test';
import { readFileSync } from 'node:fs';
import catalog from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json' with {type:'json'};
const obsolete = JSON.parse(readFileSync(new URL(`../../src/lib/politics/reform-2026/data/${catalog.defaultsPath}`,import.meta.url),'utf8'));
const base=process.env.BASE_PATH??'';
for (const version of [1,2,3,4]) test(`retired reform scenario v${version} is rejected without opening another workflow`,async({page})=>{
  await page.goto(base+'/politics');
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  await page.getByTestId('reform-file-input').setInputFiles({name:`v${version}.json`,mimeType:'application/json',buffer:Buffer.from(JSON.stringify({...obsolete,schemaVersion:version}))});
  await expect(page.getByRole('alert')).toContainText(/precedente|version|schemaVersion/i);
  await expect(page.getByLabel('Modello di generazione',{exact:true})).toHaveValue('selected');
  await expect(page.getByTestId('fit-status')).not.toHaveText('Modello pronto');
});

test('current plain scenario imported from the previous-law screen keeps its seed and generator',async({page})=>{
  const scenario=structuredClone((await currentBrowserSession()).scenario);
  scenario.seed='current-plain-import';
  await page.goto(base+'/politics');
  await page.getByTestId('scenario-file-input').setInputFiles({name:'current.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(scenario))});
  await expect(page.getByLabel('Algoritmo di scrutinio')).toHaveValue(scenario.algorithmId);
  await expect(page.getByLabel('Seme delle simulazioni',{exact:true})).toHaveValue(scenario.seed);
  await expect(page.getByLabel('Modello di generazione',{exact:true})).toHaveValue('selected');
  await expect(page.getByRole('alert')).toHaveCount(0);
});
