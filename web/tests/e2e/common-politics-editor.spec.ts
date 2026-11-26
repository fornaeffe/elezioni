import { readFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { type Page } from '@playwright/test';
import { expect, test } from '../fixtures/editor-preparation';
import authored from '../../../data/politics-default-scenario.json' with { type: 'json' };
import { decodeScenario, encodeScenario } from '../../src/lib/politics/scenario/serialization';
import { createDefaultCommonPoliticsScenario } from '../../src/lib/politics/scenario/defaults';
import { politicsDraftKey } from '../../src/lib/politics/files/draft';
import { createCandidate, upsertPlacement } from '../../src/lib/politics/scenario/edits';
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

test('visible non-valid reference retains explicit zero, default uncertainty and local tree declarations', async ({ page }) => {
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
  await expect(section.getByLabel(`SD ${referenceLabel} (punti percentuali)`, { exact: true })).toHaveValue('1');
  const emilia = section.locator('summary').filter({ hasText: /^Emilia-Romagna(?: ·|$)/ });
  await expect(emilia).toHaveText('Emilia-Romagna');
  await emilia.click();
  const province = section.locator('summary').filter({ hasText: /^Provincia 037(?: ·|$)/ });
  await expect(province).toHaveText('Provincia 037');
  await province.click();
  const local = section.getByLabel('Provincia 037 comune (%)', { exact: true });
  await local.fill('5'); await local.blur();
  await expect(province).toHaveText('Provincia 037 · 1 dichiarazioni');
  await expect(emilia).toHaveText('Emilia-Romagna · 1 dichiarazioni');
  await expect(emilia.locator('.summary-count')).toHaveCSS('font-weight', '400');
  await page.getByLabel('Camere da rinnovare').selectOption('camera');
  stored = await draft(page);
  expect(stored.nonValidReferences.some((r: { scope: { kind: string; code: string }; mean: number }) => r.scope.kind === 'province' && r.scope.code === '037' && r.mean === 0.05)).toBe(true);
  await section.getByRole('button', { name: `Rimuovi ${referenceLabel}`, exact: true }).click();
  await expect(input).toHaveValue('');
  expect((await draft(page)).nonValidReferences).toHaveLength(authored.nonValidReferences.length + 1);
  await section.getByRole('button', { name: 'Rimuovi Provincia 037 comune', exact: true }).click();
  await expect(province).toHaveText('Provincia 037');
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
  await article.locator('summary').filter({ hasText: /^Depositi e candidati$/ }).click();
  await article.locator('summary').filter({ hasText: /^Camera$/ }).click();
  const all = article.getByRole('checkbox', { name: 'Deposita tutta Camera', exact: true });
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
  await openIn(new RegExp(`^${college.name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}`));
  const candidates = summary(/^Candidati(?: ·|$)/);
  await expect(candidates.locator('.summary-count')).toHaveText('· 1 fissati');
  await expect(candidates.locator('.summary-count')).toHaveCSS('font-weight', '400');
  await candidates.click();
  const fourth = article.getByRole('combobox', { name: 'Candidato 4', exact: true });
  await expect(fourth).toHaveValue('older');
  expect(await article.getByRole('combobox', { name: 'Candidato 1', exact: true }).locator('option[value=young]').getAttribute('disabled')).not.toBeNull();
  await expect(fourth.locator('option[value=unavailable]')).toContainText('Rinuncia dichiarata');
  await expect(article.getByRole('checkbox', { name: 'Deposita tutta Camera', exact: true })).toHaveJSProperty('indeterminate', true);
  await article.getByRole('checkbox', { name: `Deposita ${college.name}`, exact: true }).uncheck();
  expect((await draft(page)).lawSettings['politiche-reform-2026-v1'].placements).toMatchObject([{ position: 4, candidateId: 'older' }]);
  await expect(article.getByText('Fissato, inattivo', { exact: true })).toBeVisible();
  await fourth.selectOption('');
  expect((await draft(page)).lawSettings['politiche-reform-2026-v1'].placements).toEqual([]);
  expect((await draft(page)).candidates).toHaveLength(3);
  for (const branch of filingPath) await expect(branch.locator('.summary-count')).toHaveCount(0);
  await expect(candidates).toHaveText('Candidati');
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
