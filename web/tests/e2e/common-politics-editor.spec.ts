import { readFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { type Page } from '@playwright/test';
import { expect, test } from '../fixtures/editor-preparation';
import authored from '../../../data/politics-default-scenario.json' with { type: 'json' };
import { decodeScenario, encodeScenario } from '../../src/lib/politics/scenario/serialization';
import { createDefaultCommonPoliticsScenario } from '../../src/lib/politics/scenario/defaults';
import { politicsDraftKey } from '../../src/lib/politics/files/draft';
import { createCandidate, selectLaw, setCoalitionScope, setFilings, setRenewal, upsertPlacement } from '../../src/lib/politics/scenario/edits';
import type { Location } from '../../src/lib/politics/scenario/schema';
import { waitForPoliticsReady } from '../fixtures/politics-readiness';

const base = process.env.BASE_PATH ?? '';
test.setTimeout(90_000);
const available = [authored.data.historical, authored.data.catalogue, authored.data.electorate,
  ...Object.values(authored.data.lawAssets).flatMap(a => [a.geography, a.seats, ...a.models.M01, ...a.models.M02])].every(p => existsSync(`static/${p.uri}`));
test.skip(!available, 'Common editor requires generated production politics bundles.');

async function ready(page: Page) {
  await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]')).toContainText('dati verificati', { timeout: 120_000 });
  await waitForPoliticsReady(page, true);
}
async function open(page: Page, title: string | RegExp) {
  const summary = page.locator('summary').filter({ hasText: title }).first();
  if (!await summary.evaluate(el => (el.parentElement as HTMLDetailsElement).open)) await summary.click();
}
async function draft(page: Page) { return page.evaluate(key => JSON.parse(localStorage.getItem(key)!), politicsDraftKey); }

test('common law/model selections, deferred settings, seeds and locks survive reload and both files', async ({ page }) => {
  await page.goto(`${base}/politics/editor`); await ready(page);
  await open(page, /^Impostazioni avanzate dello scenario$/);
  await page.getByLabel('Generatore dei voti', { exact: true }).selectOption('M01'); await ready(page);
  await page.getByRole('combobox', { name: 'Legge elettorale', exact: true }).selectOption('politiche-r-parity-v1'); await ready(page);
  await expect(page.getByLabel('Generatore dei voti', { exact: true })).toHaveValue('M01');
  await expect(page.getByLabel('Generatore dei voti').locator('option[value=M02]')).toHaveJSProperty('disabled', false);
  await expect(page.getByLabel('Generatore dei voti').locator('option[value=M02]')).toContainText('3.0.0');
  await page.getByLabel('Generatore dei voti', { exact: true }).selectOption('M02'); await ready(page);
  expect((await draft(page)).selection.generatorsByLaw['politiche-r-parity-v1']).toEqual({ id: 'M02', version: '3.0.0' });
  await page.getByRole('combobox', { name: 'Legge elettorale', exact: true }).selectOption('politiche-reform-2026-v1'); await ready(page);
  await expect(page.getByLabel('Generatore dei voti', { exact: true })).toHaveValue('M01');
  await open(page, /^Impostazioni avanzate della simulazione$/);
  await page.getByLabel('Seed del fitting', { exact: true }).fill('fit-preserved'); await page.getByLabel('Seed del fitting', { exact: true }).blur();
  const before = await draft(page);
  await page.getByRole('button', { name: 'Genera seed casuale delle simulazioni' }).click();
  let stored = await draft(page);
  expect(stored.execution.fittingSeed).toBe('fit-preserved');
  expect(stored.execution.simulationSeed).not.toBe(before.execution.simulationSeed);
  expect(stored.modelSettings).toEqual(authored.modelSettings);
  const gzipDownload = page.getByRole('button', { name: 'Scarica scenario JSON.gz', exact: true });
  const downloadOptions = page.getByRole('button', { name: 'Opzioni di download dello scenario', exact: true });
  const jsonDownload = page.getByRole('menuitem', { name: 'Scarica scenario JSON', exact: true });
  await expect(jsonDownload).toBeHidden();
  await gzipDownload.focus();
  await page.keyboard.press('Tab');
  await expect(downloadOptions).toBeFocused();
  await expect(downloadOptions).toHaveCSS('outline-style', 'solid');
  await page.keyboard.press('ArrowDown');
  await expect(downloadOptions).toHaveAttribute('aria-expanded', 'true');
  await expect(jsonDownload).toBeFocused();
  await page.keyboard.press('Escape');
  await expect(downloadOptions).toBeFocused();
  await expect(downloadOptions).toHaveCSS('outline-style', 'solid');
  await expect(jsonDownload).toBeHidden();
  await downloadOptions.click();
  await expect(jsonDownload).toBeVisible();
  await page.getByRole('heading', { name: 'Simulatore delle elezioni politiche', exact: true }).click();
  await expect(jsonDownload).toBeHidden();

  for (const gzip of [true, false]) {
    if (!gzip) await downloadOptions.click();
    const download = page.waitForEvent('download');
    if (gzip) await gzipDownload.click();
    else await jsonDownload.click();
    const file = await download, path = await file.path();
    expect(file.suggestedFilename()).toBe(gzip ? 'scenario-politiche.json.gz' : 'scenario-politiche.json');
    await expect(jsonDownload).toBeHidden();
    const bytes = new Uint8Array(await readFile(path!));
    expect(decodeScenario(bytes)).toEqual(stored);
    await page.getByLabel('Carica scenario', { exact: true }).setInputFiles({ name: file.suggestedFilename(), mimeType: 'application/octet-stream', buffer: Buffer.from(bytes) });
    await ready(page);
  }
  await page.reload(); await ready(page); await open(page, /^Impostazioni avanzate dello scenario$/);
  await expect(page.getByLabel('Generatore dei voti')).toHaveValue('M01');
  expect(await draft(page)).toEqual(stored);
});

test('non-valid reference grids reflow and preserve uncertainty and local tree declarations', async ({ page }) => {
  await page.goto(`${base}/politics/editor`); await ready(page);
  await open(page, /^Impostazioni avanzate dello scenario$/);
  const section = page.locator('details').filter({ has: page.locator(':scope > summary').filter({ hasText: /^Impostazioni avanzate dello scenario$/ }) });
  const referenceLabel = 'Astensione e voti non validi Camera';
  await section.locator('summary').filter({ hasText: /^Riferimenti avanzati/ }).click();
  const input = section.getByLabel(`${referenceLabel} (%)`, { exact: true });
  await expect(input).toBeVisible(); await expect(input).toHaveValue('');
  await input.fill('0'); await input.blur();
  await expect(section.getByLabel(`SD ${referenceLabel} (punti percentuali)`, { exact: true })).toBeDisabled();
  let stored = await draft(page);
  expect(stored.nonValidReferences.find((r: { channel: string; scope: { kind: string } }) => r.channel === 'camera' && r.scope.kind === 'national')).toMatchObject({ mean: 0, uncertainty: { sdPercentagePoints: 0, origin: 'default' } });
  await input.fill('10'); await input.blur();
  const nationalSd = section.getByLabel(`SD ${referenceLabel} (punti percentuali)`, { exact: true });
  await expect(nationalSd).toHaveValue('1');
  await nationalSd.fill('2'); await nationalSd.blur();
  await expect(nationalSd).toHaveAccessibleDescription('Esplicita');
  await section.getByRole('button', { name: `Usa SD predefinita per ${referenceLabel}`, exact: true }).click();
  await expect(nationalSd).toHaveValue('1');
  await expect(nationalSd).toHaveAccessibleDescription('Predefinita');
  await open(page, /^Lombardia$/);
  await expect(section.locator('summary').filter({ hasText: /^Provincia: Varese$/ })).toBeVisible();
  await expect(section.locator('summary').filter({ hasText: /^Provincia: Como$/ })).toBeVisible();
  const emilia = section.locator('summary').filter({ hasText: /^Emilia-Romagna(?: ·|$)/ });
  await expect(emilia).toHaveText('Emilia-Romagna');
  await emilia.click();
  const provinceLabel = 'Provincia: Bologna';
  const provinceHeading = /^Provincia: Bologna(?: ·|$)/;
  const province = section.locator('summary').filter({ hasText: provinceHeading });
  await expect(province).toHaveText(provinceLabel);
  await province.click();
  const provinceGrid = section.getByRole('group', { name: `Riferimenti per ${provinceLabel}`, exact: true });
  const local = provinceGrid.getByLabel(`${provinceLabel} comune (%)`, { exact: true });
  const localCamera = provinceGrid.getByLabel(`${provinceLabel} Camera (%)`, { exact: true });
  const localSd = provinceGrid.getByLabel(`SD ${provinceLabel} comune (punti percentuali)`, { exact: true });
  await expect(local).toHaveAttribute('placeholder', '% auto');
  await local.fill('5'); await local.blur();
  await expect(province).toHaveText(`${provinceLabel} · 1 dichiarazioni`);
  await expect(emilia).toHaveText('Emilia-Romagna · 1 dichiarazioni');
  await expect(emilia.locator('.summary-count')).toHaveCSS('font-weight', '400');
  const desktopQuota = (await local.boundingBox())!;
  const desktopCamera = (await localCamera.boundingBox())!;
  expect(desktopCamera.y).toBeCloseTo(desktopQuota.y, 0);
  expect(desktopCamera.x).toBeGreaterThan(desktopQuota.x);
  expect((await localSd.boundingBox())!.y).toBeGreaterThan(desktopQuota.y);

  await page.setViewportSize({ width: 320, height: 900 });
  const mobileQuota = (await local.boundingBox())!;
  const mobileCamera = (await localCamera.boundingBox())!;
  const mobileSd = (await localSd.boundingBox())!;
  expect(mobileCamera.x).toBeCloseTo(mobileQuota.x, 0);
  expect(mobileCamera.y).toBeGreaterThan(mobileQuota.y);
  expect(mobileSd.y).toBeCloseTo(mobileQuota.y, 0);
  expect(mobileSd.x + mobileSd.width).toBeLessThanOrEqual(320);
  await provinceGrid.getByRole('button', { name: 'Aiuto per Quota (%)', exact: true }).click();
  await expect(page.locator('[popover]:popover-open')).toContainText(/elettori.*nel territorio.*% auto.*zero è un valore esplicito/);
  await page.keyboard.press('Escape');

  const generator = section.getByLabel('Generatore dei voti', { exact: true });
  async function selectGeneratorAndOpenProvince(model: 'M01' | 'M02') {
    await generator.selectOption(model); await ready(page);
    await open(page, /^Riferimenti avanzati/);
    await open(page, /^Emilia-Romagna(?: ·|$)/);
    await open(page, provinceHeading);
  }
  await selectGeneratorAndOpenProvince('M01');
  await expect(local).toHaveValue('5');
  await expect(localSd).toHaveCount(0);
  await expect(provinceGrid.getByRole('button', { name: 'Aiuto per SD (p.p.)', exact: true })).toHaveCount(0);
  await selectGeneratorAndOpenProvince('M02');
  await expect(localSd).toHaveValue('2.5');
  await page.getByLabel('Camere da rinnovare').selectOption('camera');
  stored = await draft(page);
  expect(stored.nonValidReferences.some((r: { scope: { kind: string; code: string }; mean: number }) => r.scope.kind === 'province' && r.scope.code === '037' && r.mean === 0.05)).toBe(true);
  await section.getByRole('button', { name: `Rimuovi ${referenceLabel}`, exact: true }).click();
  await expect(input).toHaveValue('');
  expect((await draft(page)).nonValidReferences).toHaveLength(authored.nonValidReferences.length + 1);
  await section.getByRole('button', { name: `Rimuovi ${provinceLabel} comune`, exact: true }).click();
  await expect(province).toHaveText(provinceLabel);
  await expect(emilia).toHaveText('Emilia-Romagna');

  const provinceDetails = province.locator('..');
  await provinceDetails.getByRole('searchbox', { name: 'Cerca territorio' }).fill('Bologna');
  await provinceDetails.locator('summary').filter({ hasText: /^Bologna$/ }).click();
  const leaf = provinceDetails.getByRole('group', { name: 'Riferimenti per Bologna', exact: true });
  const leafQuota = leaf.getByLabel('Bologna comune (%)', { exact: true });
  await leafQuota.fill('4'); await leafQuota.blur();
  const leafSd = leaf.getByLabel('SD Bologna comune (punti percentuali)', { exact: true });
  await expect(leafSd).toHaveValue('2');
  const leafBounds = (await leafSd.boundingBox())!;
  expect(leafBounds.x + leafBounds.width).toBeLessThanOrEqual(320);
  await expect(province).toHaveText(`${provinceLabel} · 1 dichiarazioni`);

  const removeSubtree = provinceDetails.getByRole('button', { name: 'Rimuovi riferimenti nel territorio e discendenti', exact: true });
  await expect(removeSubtree).toHaveCSS('border-top-style', 'solid');
  await expect(removeSubtree.locator('svg')).toHaveAttribute('aria-hidden', 'true');
  const buttonBounds = (await removeSubtree.boundingBox())!;
  expect(buttonBounds.x + buttonBounds.width).toBeLessThanOrEqual(320);
  const labelLineCount = await removeSubtree.locator('span').evaluate(label => {
    const range = document.createRange();
    range.selectNodeContents(label);
    return range.getClientRects().length;
  });
  expect(labelLineCount).toBeGreaterThan(1);
  await removeSubtree.click();
  await expect(leafQuota).toHaveValue('');
  await expect(emilia).toHaveText('Emilia-Romagna');
});

test('new lists initialize every law, tri-state filings and linked/unlinked chamber memberships', async ({ page }) => {
  await page.goto(`${base}/politics/editor`); await ready(page);
  await page.getByLabel('Nuova lista', { exact: true }).fill('Lista nuova');
  await page.getByRole('button', { name: 'Aggiungi lista', exact: true }).click();
  let stored = await draft(page), list = stored.subjects.lists.find((l: { name: string }) => l.name === 'Lista nuova');
  expect(stored.memberships.find((m: { listId: string }) => m.listId === list.id)).toMatchObject({ camera: [], senato: [], locked: true });
  for (const block of Object.values(stored.lawSettings) as { filings: { listId: string; locations: unknown[] }[] }[]) expect(block.filings.find(f => f.listId === list.id)!.locations.length).toBeGreaterThan(0);
  const article = page.getByRole('article', { name: 'Lista nuova', exact: true });
  await article.getByRole('button', { name: /^Opzioni lista/ }).click();
  const filingSummary = article.locator('summary').filter({ hasText: /^Depositi e candidati$/ });
  const filingSection = filingSummary.locator('..');
  const wholeTree = article.getByRole('checkbox', { name: 'Deposita in tutti i collegi modificabili', exact: true });
  const checkboxLabel = wholeTree.locator('..');
  const all = article.getByRole('checkbox', { name: 'Deposita tutta Camera', exact: true });
  await expect(wholeTree).toBeVisible();
  await expect(wholeTree).toBeChecked();
  await expect(checkboxLabel).toHaveAttribute('title', 'Deposita in tutti i collegi modificabili');
  await expect(checkboxLabel).toHaveCSS('background-color', 'rgb(228, 245, 233)');
  await expect(filingSection).toHaveJSProperty('open', false);
  const marker = (await filingSummary.locator('[aria-hidden="true"]').boundingBox())!;
  const checkbox = (await checkboxLabel.boundingBox())!;
  const title = (await filingSummary.getByText('Depositi e candidati', { exact: true }).boundingBox())!;
  expect(marker.x + marker.width).toBeLessThanOrEqual(checkbox.x);
  expect(checkbox.x + checkbox.width).toBeLessThanOrEqual(title.x);
  await expect(all).toHaveCount(0);
  await wholeTree.press('Space');
  await expect(wholeTree).not.toBeChecked();
  await expect(checkboxLabel).toHaveCSS('background-color', 'rgb(255, 240, 237)');
  await expect(filingSection).toHaveJSProperty('open', false);
  await page.keyboard.press('Tab');
  await expect(filingSummary).toBeFocused();
  await filingSummary.press('Space');
  await expect(filingSection).toHaveJSProperty('open', true);
  const cameraSummary = article.locator('summary').filter({ hasText: /^Camera$/ });
  await expect(all).toBeVisible();
  await all.check();
  await expect(cameraSummary.locator('..')).toHaveJSProperty('open', false);
  await expect(wholeTree).toHaveJSProperty('indeterminate', true);
  await expect(checkboxLabel).toHaveCSS('background-color', 'rgb(255, 247, 223)');
  await wholeTree.click();
  await expect(wholeTree).toBeChecked();
  await expect(wholeTree).toHaveJSProperty('indeterminate', false);
  await expect(filingSection).toHaveJSProperty('open', true);
  await expect(all).toBeChecked();
  await all.uncheck(); stored = await draft(page);
  expect(stored.lawSettings['politiche-reform-2026-v1'].filings.find((f: { listId: string }) => f.listId === list.id).locations.every((l: { chamber: string }) => l.chamber === 'senato')).toBe(true);
  await open(page, /^Impostazioni avanzate dello scenario$/); await open(page, /^Appartenenze dei componenti$/);
  await open(page, /^Lista nuova · Camera/);
  const membership = page.locator('details').filter({ has: page.locator(':scope > summary').filter({ hasText: /^Lista nuova · Camera/ }) }).last();
  await expect(membership.getByLabel('Aggiungi componente senato')).toBeDisabled();
  await membership.getByLabel('Collega Camera e Senato').uncheck();
  await expect(membership.getByLabel('Aggiungi componente senato')).toBeEnabled();
  await membership.getByLabel('Collega Camera e Senato').check();
  await expect(membership.getByLabel('Aggiungi componente senato')).toBeDisabled();
});

test('whole-tree filings preserve excluded declarations and candidate pins', async ({ page }) => {
  const law = 'politiche-reform-2026-v1';
  const listId = 'fdi';
  let scenario = createDefaultCommonPoliticsScenario();
  const locations = scenario.lawSettings[law].filings.find(filing => filing.listId === listId)!.locations;
  const camera = locations.find(location => location.chamber === 'camera')!;
  const senate = locations.filter(location => location.chamber === 'senato');
  expect(senate.length).toBeGreaterThan(0);
  scenario = setCoalitionScope(scenario, 'centrodestra', ['camera']);
  scenario = setRenewal(scenario, 'senato');
  scenario = setFilings(scenario, law, listId, [camera, ...senate]);
  scenario = createCandidate(scenario, { id: 'pin', name: 'Candidata fissata', gender: 'female', birthDate: '1970-01-01', availability: { kind: 'available' } });
  scenario = upsertPlacement(scenario, law, {
    id: 'retained-pin', candidateId: 'pin', location: camera, owner: { kind: 'list', id: listId },
    role: 'ordinary', position: 1, affiliationListId: listId
  });
  await page.addInitScript(({ key, scenario }) => localStorage.setItem(key, JSON.stringify(scenario)), { key: politicsDraftKey, scenario });
  await page.goto(`${base}/politics/editor`); await ready(page);
  const article = page.getByRole('article', { name: "Fratelli d'Italia", exact: true });
  await article.getByRole('button', { name: /^Opzioni lista/ }).click();
  const summary = article.locator('summary').filter({ hasText: /^Depositi e candidati/ });
  const wholeTree = article.getByRole('checkbox', { name: 'Deposita in tutti i collegi modificabili', exact: true });
  await expect(wholeTree).toHaveJSProperty('indeterminate', true);
  await wholeTree.press('Space');
  await expect(wholeTree).toBeChecked();
  await expect(summary.locator('..')).toHaveJSProperty('open', false);
  const savedLocations = async (): Promise<Location[]> => (await draft(page)).lawSettings[law].filings
    .find((filing: { listId: string }) => filing.listId === listId).locations;
  const filed = await savedLocations();
  expect(filed.filter(location => location.chamber === 'camera').length).toBeGreaterThan(1);
  expect(filed.filter(location => location.chamber === 'senato')).toEqual(senate);

  await summary.click();
  const cameraSummary = article.locator('summary').filter({ hasText: /^Camera · rinnovo inattivo/ });
  await expect(article.getByRole('checkbox', { name: 'Deposita tutta Camera', exact: true })).toBeChecked();
  await expect(cameraSummary.locator('..')).toHaveJSProperty('open', false);
  await expect(article.getByRole('checkbox', { name: 'Deposita tutta Senato', exact: true })).toBeDisabled();
  await wholeTree.uncheck();
  await expect(wholeTree).toHaveJSProperty('indeterminate', false);
  expect(await savedLocations()).toEqual(senate);
  expect((await draft(page)).lawSettings[law].placements).toEqual(scenario.lawSettings[law].placements);
});

test('field toggletips explain scenario settings and work with keyboard and narrow screens', async ({ page }) => {
  await page.goto(`${base}/politics/editor`); await ready(page);
  const tip = (label: string) => page.getByRole('button', { name: `Aiuto per ${label}`, exact: true });
  const panel = page.locator('[popover]:popover-open');
  const referenceLabel = 'Riferimento nazionale (%)';
  const desktopTip = page.locator('.subject-header').filter({ has: tip(referenceLabel) }).getByRole('button');
  await desktopTip.press('Enter');
  await expect(panel).toContainText(/voti validi.*M02.*campo vuoto/);
  await page.keyboard.press('Escape');
  await expect(panel).toHaveCount(0);
  await expect(desktopTip).toBeFocused();

  await page.getByRole('button', { name: /^Opzioni lista/ }).first().click();
  await open(page, /^Riferimenti avanzati/);
  await tip('Moltiplicatore nazionale comune (adimensionale)').click();
  await expect(panel).toContainText('livello nazionale');
  await tip('Moltiplicatore locale comune (adimensionale)').click();
  await expect(panel).toContainText('da comune a comune');
  await tip('Moltiplicatore locale comune (adimensionale)').click();
  await expect(panel).toHaveCount(0);

  await open(page, /^Impostazioni avanzate dello scenario$/);
  await tip('Data di riferimento').click();
  await expect(panel).toContainText(/risultati storici e i sondaggi/);
  await page.getByLabel('Data di riferimento', { exact: true }).click();
  await expect(panel).toHaveCount(0);
  await tip('Astensione e voti non validi (%)').click();
  await expect(panel).toContainText(/elettori.*M02.*partecipazione storica/);

  await open(page, /^Identità dei candidati/); await open(page, /^Nuovo candidato$/);
  await tip('Data di nascita').click();
  await expect(panel).toContainText(/gennaio 1977.*spareggi/);
  await tip('Disponibile').click();
  await expect(panel).toContainText(/indica il motivo.*candidature già fissate/);
  await expect(page.getByRole('checkbox', { name: 'Disponibile', exact: true })).toBeChecked();

  await open(page, /^Ipotesi senza storico/);
  await page.getByRole('combobox', { name: 'Componente', exact: true }).selectOption('new:fdi');
  for (const [label, content] of [
    ['Profilo geografico', 'stesso valore in tutti i comuni'],
    ['SD profilo', 'incertezza sul profilo geografico'],
    ['SD innovazione locale', 'variazioni comunali'],
    ['SD innovazione nazionale', 'variazioni nazionali'],
  ]) {
    await tip(`${label} (log-rapporto)`).click();
    await expect(panel).toContainText(content);
  }

  await page.getByLabel('Generatore dei voti', { exact: true }).selectOption('M01'); await ready(page);
  await tip('Astensione e voti non validi (%)').click();
  await expect(panel).toContainText(/elettori.*M01.*partecipazione storica/);
  await page.keyboard.press('Escape');
  await page.setViewportSize({ width: 320, height: 660 });
  await expect(desktopTip).toBeHidden();
  const mobileTip = page.locator('.subject-reference').first().getByRole('button', { name: `Aiuto per ${referenceLabel}`, exact: true });
  await mobileTip.click();
  await expect(panel).toContainText(/voti validi.*M01.*campo vuoto/);
  expect(await panel.evaluate(el => {
    const { left, right, top, bottom } = el.getBoundingClientRect();
    return left >= 0 && right <= innerWidth && top >= 0 && bottom <= innerHeight;
  })).toBe(true);

  await tip('Numero di simulazioni').click();
  await expect(panel).toContainText(/10 simulazioni.*1\.000/);
});

test('candidate creation has no inferred gender, preserves assumed dates and supports edits/deletion', async ({ page }) => {
  await page.goto(`${base}/politics/editor`); await ready(page);
  await open(page, /^Impostazioni avanzate dello scenario$/); await open(page, /^Identità dei candidati/); await open(page, /^Nuovo candidato$/);
  await expect(page.getByLabel('Genere', { exact: true })).toHaveValue('');
  await page.getByLabel('Nome candidato').fill('Candidata dichiarata');
  await page.getByLabel('Genere', { exact: true }).selectOption('female');
  await page.getByRole('button', { name: 'Salva candidato', exact: true }).click();
  const stored = await draft(page);
  expect(stored.candidates[0]).toMatchObject({ name: 'Candidata dichiarata', birthDate: '1977-01-01', birthDateOrigin: 'assumed' });
  await page.getByLabel('Data elezione', { exact: true }).fill('2028-09-25'); await page.getByLabel('Data elezione', { exact: true }).blur();
  expect((await draft(page)).candidates[0].birthDate).toBe('1977-01-01');
  await open(page, /^Candidata dichiarata$/);
  await page.getByRole('button', { name: 'Elimina identità e tutte le candidature' }).click();
  expect((await draft(page)).candidates).toEqual([]);
});

test('invalid imports and missing pins leave the draft and old keys intact; reset is explicit', async ({ page }) => {
  await page.goto(`${base}/politics/editor`); await ready(page);
  const name = page.getByLabel('Nome scenario', { exact: true });
  await name.fill('Conservare'); await name.blur();
  await page.evaluate(() => localStorage.setItem('elezioni:politics:legacy', 'untouched'));
  const before = await draft(page);
  await page.getByLabel('Carica scenario', { exact: true }).setInputFiles({ name: 'old.json', mimeType: 'application/json', buffer: Buffer.from('{"schemaVersion":5,"kind":"old-politics"}') });
  await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]')).toContainText('Scenario non caricato');
  expect(await draft(page)).toEqual(before);
  const invalid = createDefaultCommonPoliticsScenario();
  const changed = { ...invalid, name: 'Non applicare', data: { ...invalid.data, historical: { ...invalid.data.historical, uri: 'data/not-here.json' } } };
  await page.getByLabel('Carica scenario', { exact: true }).setInputFiles({ name: 'missing.json.gz', mimeType: 'application/gzip', buffer: Buffer.from(encodeScenario(changed, true)) });
  // File metadata is streamed while the selected model may still be fitting.
  await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]')).toContainText('Dati esatti non disponibili', { timeout: 30_000 });
  expect(await draft(page)).toEqual(before);
  expect(await page.evaluate(() => localStorage.getItem('elezioni:politics:legacy'))).toBe('untouched');
  page.once('dialog', d => d.dismiss()); await page.getByRole('button', { name: 'Ripristina scenario predefinito' }).click();
  await expect(name).toHaveValue('Conservare');
  page.once('dialog', d => d.accept()); await page.getByRole('button', { name: 'Ripristina scenario predefinito' }).click();
  await expect(name).toHaveValue(authored.name);
});

test('mobile layout mounts expensive trees only on demand and permits keyboard editing', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  const errors: string[] = []; page.on('pageerror', e => errors.push(e.message));
  await page.goto(`${base}/politics/editor`); await ready(page);
  expect(await page.locator('select').count()).toBeLessThan(50);
  await expect(page.getByLabel('Generatore dei voti')).toHaveCount(0);
  const name = page.getByLabel('Nome scenario', { exact: true }); await name.focus();
  await page.keyboard.press('ControlOrMeta+A'); await page.keyboard.type('Modifica tastiera'); await page.keyboard.press('Tab');
  expect((await draft(page)).name).toBe('Modifica tastiera');
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  expect(errors).toEqual([]);
  await page.screenshot({ path: 'test-results/common-editor-mobile.png', fullPage: true });
  await page.screenshot({ path: 'test-results/common-editor-mobile-viewport.png' });
});

test('bonus declarations remain visible when inactive, reactivate, disclose replacement and clean up on deletion', async ({ page }) => {
  await page.goto(`${base}/politics/editor`); await ready(page);
  // Empty coalitions still have declaration-based link eligibility; no invented list participation.
  for (const name of ['Accordo Camera', 'Accordo Senato', 'Alternativa Senato']) {
    await page.getByLabel('Nuova coalizione', { exact: true }).fill(name);
    await page.getByRole('button', { name: 'Aggiungi coalizione', exact: true }).click();
    const article = page.getByRole('article', { name, exact: true });
    await article.getByRole('button', { name: /^Opzioni coalizione/ }).click();
    await article.getByLabel(name === 'Accordo Camera' ? 'Senato' : 'Camera', { exact: true }).uncheck();
  }
  const camera = page.getByRole('article', { name: 'Accordo Camera', exact: true });
  const senato = page.getByRole('article', { name: 'Accordo Senato', exact: true });
  const option = await camera.getByLabel('Controparte Senato').getByRole('option', { name: 'Accordo Senato (coalizione)', exact: true }).getAttribute('value');
  await camera.getByLabel('Controparte Senato').selectOption(option!);
  await camera.getByRole('button', { name: 'Conferma collegamento', exact: true }).click();
  await expect(camera.getByText(/Esplicito attivo/)).toBeVisible();
  await camera.getByLabel('Senato', { exact: true }).check();
  await expect(camera.getByText(/Esplicito conservato, inattivo/)).toBeVisible();
  await expect(camera.getByText(/Automatico/)).toBeVisible();
  await expect(camera.getByLabel('Controparte Senato')).toHaveCount(0);
  await camera.getByLabel('Senato', { exact: true }).uncheck();
  await expect(camera.getByText(/Esplicito attivo/)).toBeVisible();
  const alternate = await camera.getByLabel('Controparte Senato').getByRole('option', { name: 'Alternativa Senato (coalizione)', exact: true }).getAttribute('value');
  await camera.getByLabel('Controparte Senato').selectOption(alternate!);
  await expect(camera.getByRole('status')).toContainText('Saranno sostituiti: Accordo Camera ↔ Accordo Senato');
  await camera.getByRole('button', { name: 'Conferma collegamento', exact: true }).click();
  expect((await draft(page)).lawSettings['politiche-reform-2026-v1'].bonusLinks).toHaveLength(1);
  await camera.getByRole('button', { name: 'Elimina coalizione', exact: true }).click();
  expect((await draft(page)).lawSettings['politiche-reform-2026-v1'].bonusLinks).toEqual([]);
  await expect(senato).toBeVisible();
});

test('coalition candidate trees prune empty Reform branches and keep Rosatellum paths', async ({ page }) => {
  await page.goto(`${base}/politics/editor`); await ready(page);
  const coalition = page.getByRole('article', { name: 'Centrodestra', exact: true });
  const summary = (title: string) => coalition.locator('summary').and(coalition.getByText(title, { exact: true }));
  const firstSlot = coalition.getByRole('combobox', { name: 'Candidato 1', exact: true });

  await coalition.getByRole('button', { name: 'Opzioni coalizione Centrodestra', exact: true }).click();
  await summary('Candidati della coalizione').click();
  await summary('Camera').click();
  const territory = summary('Piemonte 1');
  await territory.click();
  await expect(territory.locator('..').locator(':scope > details > summary')).toHaveText(['Candidati']);
  await summary('Candidati').click();
  await expect(firstSlot).toBeVisible();
  await firstSlot.press('ArrowDown');
  const options = page.getByRole('listbox');
  await expect(options.getByRole('option', { name: 'Genera automaticamente' })).toBeVisible();
  await expect(options.getByRole('option', { name: 'Crea candidato' })).toHaveCount(0);
  await firstSlot.press('Escape');

  await page.getByRole('combobox', { name: 'Legge elettorale', exact: true }).selectOption('politiche-r-parity-v1');
  await ready(page);
  await summary('Candidati della coalizione').click();
  await summary('Camera').click();
  await summary('Collegio plurinominale Piemonte 1 - P01').click();
  await summary('Collegio uninominale Piemonte 1 - U01').click();
  await summary('Candidati').click();
  await expect(firstSlot).toBeVisible();
});

test('candidate trees retain sparse pins and explain disabled identities while filings become inactive', async ({ page }) => {
  let s = createDefaultCommonPoliticsScenario();
  s = createCandidate(s, { id: 'older', name: 'Identità eleggibile', gender: 'female', birthDate: '1970-01-01', availability: { kind: 'available' } });
  s = createCandidate(s, { id: 'young', name: 'Identità troppo giovane', gender: 'female', birthDate: '2020-01-01', availability: { kind: 'available' } });
  s = createCandidate(s, { id: 'unavailable', name: 'Identità indisponibile', gender: 'male', birthDate: '1970-01-01', availability: { kind: 'unavailable', reason: 'Rinuncia dichiarata' } });
  const location = s.lawSettings['politiche-reform-2026-v1'].filings.find(f => f.listId === 'fdi')!.locations.find(l => l.chamber === 'camera')!;
  s = upsertPlacement(s, 'politiche-reform-2026-v1', { id: 'pinned-four', candidateId: 'older', location, owner: { kind: 'list', id: 'fdi' }, role: 'ordinary', position: 4, affiliationListId: 'fdi' });
  await page.addInitScript(({ key, s }) => localStorage.setItem(key, JSON.stringify(s)), { key: politicsDraftKey, s });
  const seats = JSON.parse(await readFile(`static/${authored.data.lawAssets['politiche-reform-2026-v1'].seats.uri}`, 'utf8'));
  const college = seats.geography.colleges.find((c: { id: string }) => c.id === location.code);
  const territory = seats.geography.territories.find((t: { id: string }) => t.id === college.territoryId);
  await page.goto(`${base}/politics/editor`); await ready(page);
  const article = page.getByRole('article', { name: "Fratelli d'Italia", exact: true });
  const summary = (pattern: RegExp) => article.locator('summary').filter({ hasText: pattern }).first();
  const filingPath: ReturnType<typeof summary>[] = [];
  const openIn = async (pattern: RegExp) => {
    const branch = summary(pattern);
    await expect(branch.locator('.summary-count')).toHaveText('· 1 candidati fissati');
    filingPath.push(branch);
    await branch.click();
  };
  await article.getByRole('button', { name: /^Opzioni lista/ }).click();
  await openIn(/^Depositi e candidati/); await openIn(/^Camera(?: ·|$)/);
  await openIn(new RegExp(`^${territory.name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}`));
  const collegeSummary = summary(new RegExp(`^${college.name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}`));
  const collegeCheckbox = article.getByRole('checkbox', { name: `Deposita ${college.name}`, exact: true });
  await expect(collegeSummary.locator('..')).toHaveJSProperty('open', false);
  await expect(collegeCheckbox).toBeChecked();
  await expect(collegeCheckbox.locator('..')).toHaveAttribute('title', `Deposita in ${college.name}`);
  await openIn(new RegExp(`^${college.name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}`));
  const candidates = summary(/^Candidati(?: ·|$)/);
  await expect(candidates.locator('.summary-count')).toHaveText('· 1 fissati');
  await expect(candidates.locator('.summary-count')).toHaveCSS('font-weight', '400');
  await candidates.click();
  const fourth = article.getByRole('combobox', { name: 'Candidato 4', exact: true });
  const options = page.getByRole('listbox');
  await expect(fourth).toHaveValue('Identità eleggibile');
  await article.getByRole('combobox', { name: 'Candidato 1', exact: true }).press('ArrowDown');
  await expect(options.getByRole('option', { name: /Identità troppo giovane/ })).toBeDisabled();
  await expect(options.getByRole('option', { name: /Identità troppo giovane/ })).toContainText('Età insufficiente');
  await expect(options.getByRole('option', { name: /Identità indisponibile/ })).toBeDisabled();
  await expect(options.getByRole('option', { name: /Identità indisponibile/ })).toContainText('Rinuncia dichiarata');
  await expect(options.getByRole('option', { name: /Identità eleggibile/ })).toBeDisabled();
  await page.keyboard.press('Escape');
  await expect(article.getByRole('checkbox', { name: 'Deposita tutta Camera', exact: true })).toHaveJSProperty('indeterminate', true);
  await collegeCheckbox.uncheck();
  expect((await draft(page)).lawSettings['politiche-reform-2026-v1'].placements).toMatchObject([{ position: 4, candidateId: 'older' }]);
  await expect(article.getByText('Fissato, inattivo', { exact: true })).toBeVisible();
  await fourth.press('ArrowDown');
  await options.getByRole('option', { name: 'Genera automaticamente' }).click();
  await expect(fourth).toHaveValue('Genera automaticamente');
  expect((await draft(page)).lawSettings['politiche-reform-2026-v1'].placements).toEqual([]);
  expect((await draft(page)).candidates).toHaveLength(3);
  for (const branch of filingPath) await expect(branch.locator('.summary-count')).toHaveCount(0);
  await expect(candidates).toHaveText('Candidati');
});

test('candidate picker searches and creates atomically with cancellation and focus return', async ({ page }) => {
  const s = createCandidate(selectLaw(createDefaultCommonPoliticsScenario(), 'politiche-r-parity-v1'), {
    id: 'existing', name: 'Ada Esistente', gender: 'female', birthDate: '1970-01-01', availability: { kind: 'available' },
  });
  await page.addInitScript(({ key, s }) => localStorage.setItem(key, JSON.stringify(s)), { key: politicsDraftKey, s });
  await page.goto(`${base}/politics/editor`); await ready(page);
  const coalition = page.getByRole('article', { name: 'Centrodestra', exact: true });
  const summary = (title: string) => coalition.locator('summary').and(coalition.getByText(title, { exact: true }));
  await coalition.getByRole('button', { name: 'Opzioni coalizione Centrodestra', exact: true }).click();
  for (const title of ['Candidati della coalizione', 'Camera', 'Collegio plurinominale Piemonte 1 - P01', 'Collegio uninominale Piemonte 1 - U01', 'Candidati']) {
    await summary(title).click();
  }

  const slot = coalition.getByRole('combobox', { name: 'Candidato 1', exact: true });
  const options = page.getByRole('listbox');
  await slot.fill('esIST');
  await expect(options.getByRole('option', { name: 'Ada Esistente' })).toBeVisible();
  await expect(options.getByRole('option', { name: 'Genera automaticamente' })).toHaveCount(0);
  await slot.press('Enter');
  await expect(slot).toHaveValue('Ada Esistente');
  const before = await draft(page);
  const previousPin = before.lawSettings['politiche-r-parity-v1'].placements[0];
  expect(previousPin).toMatchObject({ candidateId: 'existing', position: 1, role: 'uninominal' });

  await slot.fill('Non corrisponde');
  await expect(options.getByRole('status')).toHaveText('Nessuna opzione trovata');
  await slot.press('Escape');
  await expect(options).toBeHidden();
  await expect(slot).toHaveValue('Ada Esistente');
  expect(await draft(page)).toEqual(before);

  const dialog = page.getByRole('dialog', { name: 'Crea candidato' });
  const name = dialog.getByRole('textbox', { name: 'Nome candidato' });
  await slot.fill('Nuova');
  await expect(options.getByRole('option', { name: 'Crea candidato' })).toBeVisible();
  await slot.press('Enter');
  await expect(dialog).toBeVisible();
  await expect(name).toBeFocused();
  await name.fill('Da scartare');
  await page.keyboard.press('Escape');
  await expect(dialog).toBeHidden();
  await expect(slot).toBeFocused();
  await expect(slot).toHaveValue('Ada Esistente');
  expect(await draft(page)).toEqual(before);

  await slot.press('ArrowDown');
  await options.getByRole('option', { name: 'Crea candidato' }).click();
  await expect(name).toHaveValue('');
  await dialog.getByRole('button', { name: 'Annulla' }).click();
  await expect(dialog).toBeHidden();
  await expect(slot).toBeFocused();

  await slot.press('ArrowDown');
  await options.getByRole('option', { name: 'Crea candidato' }).click();
  await name.fill('Teresa Nuova');
  await dialog.getByRole('combobox', { name: 'Genere' }).selectOption('female');
  const birthDate = dialog.getByLabel('Data di nascita', { exact: true });
  await birthDate.fill('2020-01-01');
  const save = dialog.getByRole('button', { name: 'Salva candidato' });
  await save.click();
  await expect(dialog.getByRole('alert')).toHaveText('Età insufficiente per la camera');
  await expect(dialog).toBeVisible();
  await expect(name).toHaveValue('Teresa Nuova');
  expect(await draft(page)).toEqual(before);

  await birthDate.fill('1970-01-01');
  await save.click();
  await expect(dialog).toBeHidden();
  await expect(slot).toBeFocused();
  await expect(slot).toHaveValue('Teresa Nuova');
  const after = await draft(page);
  expect(after.candidates).toHaveLength(2);
  const created = after.candidates[1];
  expect(created).toMatchObject({ name: 'Teresa Nuova', birthDate: '1970-01-01' });
  expect(after.lawSettings['politiche-r-parity-v1'].placements).toEqual([{ ...previousPin, candidateId: created.id }]);

  await page.setViewportSize({ width: 375, height: 800 });
  await coalition.getByRole('button', { name: 'Mostra opzioni per Candidato 1' }).click();
  await expect(options.getByRole('option', { name: 'Teresa Nuova' })).toBeVisible();
  const inputBox = (await slot.boundingBox())!;
  const popupBox = (await options.boundingBox())!;
  expect(Math.abs(popupBox.x - inputBox.x)).toBeLessThan(8);
  expect(popupBox.x).toBeGreaterThanOrEqual(0);
  expect(popupBox.x + popupBox.width).toBeLessThanOrEqual(375);
});

test('advanced controls edit source membership, pooling provenance and model-specific fallback without losing inactive values', async ({ page }) => {
  await page.goto(`${base}/politics/editor`); await ready(page);
  await open(page, /^Impostazioni avanzate dello scenario$/);
  await open(page, /^Raggruppamento dei componenti/);
  const pool = page.locator('details').filter({ has: page.locator(':scope > summary').filter({ hasText: /historical-rest/ }) }).last();
  await pool.locator(':scope > summary').click();
  await pool.getByLabel('Provenienza raggruppamento').fill('Provenienza verificata'); await pool.getByLabel('Provenienza raggruppamento').blur();
  expect((await draft(page)).history.pooling[0].provenance).toBe('Provenienza verificata');
  await open(page, /^Ipotesi senza storico/);
  await page.getByRole('combobox', { name: 'Componente', exact: true }).selectOption('new:fdi');
  await page.getByLabel('SD profilo (log-rapporto)', { exact: true }).fill('0.7'); await page.getByLabel('SD profilo (log-rapporto)', { exact: true }).blur();
  expect((await draft(page)).newComponents.find((c: { id: string }) => c.id === 'new:fdi').fallback.M02.profileSd).toBe(0.7);
  await page.getByLabel('Generatore dei voti', { exact: true }).selectOption('M01'); await ready(page);
  await open(page, /^Ipotesi senza storico/); await page.getByRole('combobox', { name: 'Componente', exact: true }).selectOption('new:fdi');
  await expect(page.getByText(/M01 usa il profilo regionale/)).toBeVisible();
  expect((await draft(page)).newComponents.find((c: { id: string }) => c.id === 'new:fdi').fallback.M02.profileSd).toBe(0.7);
  await open(page, /^Appartenenze dei componenti$/);
  await page.getByLabel('Vista appartenenze').selectOption('source');
  await page.getByRole('combobox', { name: 'Fonte', exact: true }).selectOption('camera-2008');
  const choice = authored.history.sourceMemberships.find(r => r.sourceId === 'camera-2008')!;
  await page.getByRole('combobox', { name: 'Opzione', exact: true }).selectOption(choice.choiceId);
  const memberId = choice.members[0];
  const memberName = authored.subjects.components.find(c => c.id === memberId)?.name ?? memberId;
  await page.getByRole('button', { name: `Rimuovi membro ${memberName}`, exact: true }).click();
  expect((await draft(page)).history.sourceMemberships.find((r: { choiceId: string }) => r.choiceId === choice.choiceId).members).toEqual([]);
  await open(page, /^Elettorato/);
  await expect(page.getByText(/età minima 18 anni/)).toBeVisible();
});


test('explicit worker requests retain CPU choices and expose memory warnings', async ({ page }) => {
    await page.addInitScript(() => { Object.defineProperty(navigator, 'hardwareConcurrency', { get: () => 28 }); Object.defineProperty(navigator, 'deviceMemory', { get: () => 64 }); });
    await page.goto(`${process.env.BASE_PATH ?? ''}/politics/editor`); await waitForPoliticsReady(page);
    await page.getByText('Impostazioni avanzate della simulazione', { exact: true }).click();
    await page.getByRole('combobox', { name: 'Worker richiesti', exact: true }).selectOption('fixed');
    const workers = page.getByLabel('Numero di worker richiesti', { exact: true });
    await expect(workers).toBeVisible();
    await workers.fill('26');
    await workers.blur();
    await waitForPoliticsReady(page);
    await expect(page.locator('.worker-warning')).toContainText('riducendo il numero di worker');
    await expect(workers).toHaveValue('26');
});
