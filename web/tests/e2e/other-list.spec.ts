import {openCommon,runCommon,downloadCommon} from '../fixtures/common-page';
import {ROSATELLUM,REFORM} from '../../src/lib/politics/scenario/schema';
import {gunzipSync} from 'fflate';
import { expect, test } from '@playwright/test';
import { politicsTestMs, waitForPoliticsReady } from '../fixtures/politics-readiness';
test.setTimeout(politicsTestMs);

for (const route of ['emilia-romagna']) {
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
    const result = page.getByRole('table', { name: 'Regional average seats by list', exact: true });
    await expect(result).toBeVisible({ timeout: 60_000 });
    for (const r of await result.getByRole('row').filter({ hasText: 'Altre liste' }).all()) await expect(r).toContainText('0');
    await toggle.uncheck();
    await expect(row).toHaveCount(0);
    await toggle.check();
    await expect(row).toHaveCount(1);
    await expect(row.getByLabel('Usa quota Altre liste')).not.toBeChecked();
  });
}


for (const [law,model] of [[ROSATELLUM,'M01'],[REFORM,'M01'],[REFORM,'M02']] as const) {
 test(`${law}/${model}: residual creation, zero seats, persistence and fresh reactivation`,{ tag: '@production' },async({page})=>{
  // Several edits intentionally invalidate the fit; budget their bounded waits
  // and executions together, rather than ending halfway through a valid fit.
  if (model === 'M02') test.setTimeout(15 * 60_000);
  const errors:string[]=[];page.on('pageerror',e=>errors.push(e.message));
  await openCommon(page,law,model);
  const row=page.getByRole('article',{name:'Altre liste',exact:true});
  await row.getByRole('button',{name:'Attiva',exact:true}).click();
  const activate=row.getByRole('button',{name:'Attiva Altre liste',exact:true});
  await expect(activate).toBeDisabled();
  const members=row.getByRole('group',{name:'Componenti del residuo'});
  const first=members.getByRole('checkbox').first();
  const id=await first.getAttribute('value');
  await first.check();
  await expect(row.getByText('1 selezionati')).toBeVisible();
  await members.getByRole('searchbox').fill('nessun componente corrispondente');
  await expect(members.getByText('Nessun componente corrisponde alla ricerca.')).toBeVisible();
  await members.getByRole('searchbox').clear();
  await expect(first).toBeChecked();
  await page.setViewportSize({width:390,height:844});
  expect(await row.evaluate(e => e.scrollWidth)).toBeLessThanOrEqual(await row.evaluate(e => e.clientWidth));
  await activate.click();
  await page.setViewportSize({width:1280,height:720});
  await expect(row.locator('input[type=color], input[type=text], select')).toHaveCount(0);
  const reference=row.getByLabel('Riferimento nazionale (%)',{exact:true});
  await reference.fill('10');await reference.blur();
  const result=await runCommon(page);
  const file=await downloadCommon(page,'Scarica archivio risultati JSON.gz');
  const archive=JSON.parse(new TextDecoder().decode(gunzipSync(file)));
  const series=archive.run.result.tables.find((t:any)=>t.name==='Reform statistical series v1');
  const residual=series.rows.filter((r:any[])=>r[series.columns.indexOf('Owner')]==='list:__other__');
  expect(residual.length).toBeGreaterThan(0);expect(residual.every((r:any[])=>r[series.columns.indexOf('Elected')]===0)).toBe(true);
  expect(result.politics.run.session.simulations).toEqual([{id:1,outcome:'complete'}]);
  await page.reload();await waitForPoliticsReady(page);await expect(row).toHaveCount(1);await expect(reference).toHaveValue('10');
  await page.getByLabel('Legge elettorale').selectOption(law===REFORM?ROSATELLUM:REFORM);
  await waitForPoliticsReady(page, true);
  await expect(row).toHaveCount(1);
  await row.getByRole('button',{name:/Opzioni/}).click();
  await expect(row.getByText(/Camera e Senato collegati/)).toBeVisible();
  await row.getByText(/^Componenti · Camera e Senato collegati/).click();
  await row.getByText(/^Altre liste · Camera/).click();
  await expect(row.getByLabel('Collega Camera e Senato')).toBeChecked();
  await row.getByText('Copertura territoriale', {exact:true}).click();
  await expect(row.getByText(/La copertura determina/)).toBeVisible();
  await row.getByRole('button',{name:'Disattiva Altre liste',exact:true}).click();
  await expect(row.getByText(/non ripristina i riferimenti precedenti/)).toBeVisible();
  await row.getByRole('button',{name:'Conferma disattivazione'}).click();
  await row.getByRole('button',{name:'Attiva',exact:true}).click();
  await row.locator(`input[type=checkbox][value="${id}"]`).check();
  await activate.click();
  await expect(reference).toHaveValue('');expect(errors).toEqual([]);
 });
}
