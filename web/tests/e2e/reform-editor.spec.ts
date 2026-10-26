import { readFileSync } from 'node:fs';
import { expect, test, type Page } from '@playwright/test';
import catalog from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json' with { type: 'json' };
import type { ReformGenerationScenario } from '../../src/lib/politics/reform-2026/generation-types';
const defaults: ReformGenerationScenario = JSON.parse(readFileSync(new URL(`../../src/lib/politics/reform-2026/data/${catalog.defaultsPath}`, import.meta.url), 'utf8'));
const editorCatalog = JSON.parse(readFileSync(new URL(`../../src/lib/politics/reform-2026/data/${catalog.editorPath}`, import.meta.url), 'utf8'));
const key = 'elezioni:politics-reform-2026:scenario:v1';
test('poll reference v3 survives JSON import, ordinary editing, reload and export', async ({ page }) => {
  await open(page);
  const scenario: ReformGenerationScenario = { ...structuredClone(defaults), schemaVersion: 3,
    pollReferences: { version: 'constructed-poll-reference', archiveVersion: 'constructed-archive', policyVersion: 'poll-reference-v1', date: defaults.projectionDate,
      sourceQuestionIds: ['constructed-question'], assumptions: ['Constructed browser compatibility fixture.'],
      rows: (['camera', 'senato'] as const).map(chamber => ({ chamber, listId: defaults.lists[chamber][0].id, share: 1, sigma: .1, donorHistoricalListIds: [] })) } };
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'polls.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(scenario)) });
  await expect.poll(async () => (await stored(page)).schemaVersion).toBe(3);
  await page.getByLabel('Nome scenario', { exact: true }).fill('Con sondaggi');
  await page.getByLabel('Nome scenario', { exact: true }).press('Tab');
  expect((await stored(page)).pollReferences).toEqual(scenario.pollReferences);
  await page.reload(); await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue('Con sondaggi');
  const download = page.waitForEvent('download'); await page.getByRole('button', { name: 'Scarica scenario', exact: true }).click();
  const stream = await (await download).createReadStream(), chunks: Buffer[] = []; for await (const chunk of stream!) chunks.push(Buffer.from(chunk));
  expect(JSON.parse(Buffer.concat(chunks).toString()).pollReferences).toEqual(scenario.pollReferences);
});
async function open(page: Page) {
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  await expect(page.getByRole('button', { name: 'Esegui riforma', exact: true })).toBeEnabled();
}
async function stored(page: Page): Promise<ReformGenerationScenario> { return page.evaluate(key => JSON.parse(localStorage.getItem(key)!), key); }

test('common editing is the default, chamber overrides are explicit, and presentation survives reload/export', async ({ page }) => {
  const errors: string[] = []; page.on('pageerror', e => errors.push(e.message));
  await open(page);
  await expect(page.getByRole('combobox', { name: 'Ramo riferimenti', exact: true })).toHaveValue('common');
  await expect(page.getByRole('button', { name: 'Camera', exact: true })).toHaveCount(0);
  const lead = defaults.lists.camera[0];
  await page.getByLabel('Nome scenario', { exact: true }).fill('Scenario condiviso');
  await page.getByLabel('Nome scenario', { exact: true }).press('Tab');
  await page.getByLabel(`Quota ${lead.name}`, { exact: true }).fill('37');
  await expect.poll(async () => (await stored(page)).shares.find(r => r.listId === lead.id)?.common).toBe(.37);
  await page.getByRole('combobox', { name: 'Ramo riferimenti', exact: true }).selectOption('senato');
  await page.getByLabel(`Quota ${lead.name}`, { exact: true }).fill('39');
  await page.getByRole('combobox', { name: 'Ramo riferimenti', exact: true }).selectOption('common');
  await page.getByLabel(`Quota ${lead.name}`, { exact: true }).fill('0');
  await expect.poll(async () => (await stored(page)).shares.find(r => r.listId === lead.id)).toMatchObject({ common: 0, camera: null, senato: .39 });
  await page.getByLabel(`Quota ${lead.name}`, { exact: true }).fill('');
  await expect.poll(async () => (await stored(page)).shares.find(r => r.listId === lead.id)).toMatchObject({ common: null, senato: .39 });
  const row = page.getByTestId('reform-list-row').filter({ has: page.getByLabel(`Colore lista ${lead.name}`, { exact: true }) });
  await row.getByLabel('Nome lista', { exact: true }).fill('Lista comune'); await row.getByLabel('Nome lista', { exact: true }).press('Tab');
  await expect.poll(async () => (await stored(page)).lists.senato.find(l => l.id === lead.id)?.name).toBe('Lista comune');
  const payload = await stored(page);
  expect(payload.lists.camera.find(l => l.id === lead.id)?.name).toBe('Lista comune');
  await page.reload(); await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue('Scenario condiviso');
  await expect(page.getByRole('button', { name: 'Camera', exact: true })).toHaveCount(0);
  const download = page.waitForEvent('download'); await page.getByRole('button', { name: 'Scarica scenario', exact: true }).click();
  const stream = await (await download).createReadStream(), chunks: Buffer[] = []; for await (const chunk of stream!) chunks.push(Buffer.from(chunk));
  expect(JSON.parse(Buffer.concat(chunks).toString())).toEqual(payload);
  await page.setViewportSize({ width: 390, height: 844 });
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  await page.screenshot({ path: 'test-results/reform-editor-mobile.png', fullPage: true });
  expect(errors).toEqual([]);
});

test('dependency removal is reviewable and invalid drafts do not overwrite the last valid saved scenario', async ({ page }) => {
  await open(page);
  const lead = defaults.lists.camera[0];
  await page.getByRole('button', { name: `Rimuovi lista ${lead.name}`, exact: true }).click();
  await expect(page.getByRole('button', { name: 'Applica modifica e rimuovi dipendenze' })).toBeVisible();
  expect((await stored(page)).lists.camera.some(l => l.id === lead.id)).toBe(true);
  await page.getByRole('button', { name: 'Mantieni scenario' }).click();
  await page.getByRole('button', { name: `Rimuovi lista ${lead.name}`, exact: true }).click();
  await page.getByRole('button', { name: 'Applica modifica e rimuovi dipendenze' }).click();
  await expect.poll(async () => (await stored(page)).lists.camera.some(l => l.id === lead.id)).toBe(false);
  await page.getByLabel('Data proiezione', { exact: true }).fill('2099-01-01'); await page.getByLabel('Data proiezione', { exact: true }).press('Tab');
  await expect(page.getByTestId('reform-scenario-validation')).toContainText('Projection date is after election date');
  await expect(page.getByRole('button', { name: 'Esegui riforma', exact: true })).toBeDisabled();
  expect((await stored(page)).projectionDate).toBe(defaults.projectionDate);
});

test('version 1 JSON imports upgrade the editable draft and supplied data stays in JSON', async ({ page }) => {
  await open(page);
  const old = { ...defaults, schemaVersion: 1 }; delete old.presentation;
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'v1.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(old)) });
  await expect.poll(async () => (await stored(page)).schemaVersion).toBe(2);
  await page.getByText('Scenario completo e dati forniti (JSON)', { exact: true }).click();
  await page.getByRole('button', { name: 'Modifica scenario completo', exact: true }).click();
  const supplied = structuredClone(await stored(page));
  const coverage = supplied.coverage.find(c => c.chamber === 'camera' && c.listIds.length)!;
  supplied.suppliedPreferences = [{ chamber: 'camera', collegeId: coverage.collegeId, listId: coverage.listIds[0], preferences: { kind: 'missing', reason: 'Non disponibili' } }];
  await page.getByLabel('Scenario completo JSON').fill(JSON.stringify(supplied));
  await page.getByRole('button', { name: 'Applica scenario completo', exact: true }).click();
  await expect.poll(async () => (await stored(page)).suppliedPreferences.length).toBe(1);
  await page.getByRole('button', { name: 'Aggiungi lista', exact: true }).click();
  await expect(page.getByRole('alert')).toContainText('JSON completo');
});

test('production worker previews actual bonus prefixes and accepts a compatible pinned candidate', async ({ page }) => {
  test.setTimeout(90000);
  const errors: string[] = []; page.on('pageerror', e => errors.push(e.message));
  await open(page);
  await page.getByText('Candidature e anteprima del premio', { exact: true }).click();
  await page.getByRole('combobox', { name: 'Ruolo candidatura', exact: true }).selectOption('bonus');
  const territory = editorCatalog.territories.find((t: { chamber: string; kind: string }) => t.chamber === 'camera' && t.kind === 'ordinary');
  await page.getByRole('combobox', { name: 'collegio o territorio candidatura', exact: true }).selectOption(territory.id);
  const lead = defaults.lists.camera[0];
  await page.getByRole('combobox', { name: 'Titolare candidatura', exact: true }).selectOption(lead.coalitionId ?? lead.id);
  await page.getByRole('button', { name: 'Anteprima posizioni', exact: true }).click();
  await expect(page.getByRole('table', { name: 'Posizioni pianificate', exact: true })).toBeVisible({ timeout: 60000 });
  await page.getByRole('button', { name: 'Usa posizione 1', exact: true }).click();
  await page.getByLabel('Nome candidato', { exact: true }).fill('Persona di prova');
  await page.getByLabel('Data nascita', { exact: true }).fill('1970-01-01');
  await page.getByRole('button', { name: 'Salva template candidato', exact: true }).click();
  await expect.poll(async () => (await stored(page)).candidateTemplates.length).toBe(1);
  await page.getByRole('button', { name: 'Anteprima posizioni', exact: true }).click();
  await expect(page.getByRole('table', { name: 'Posizioni pianificate', exact: true })).toContainText('Persona di prova', { timeout: 60000 });
  await expect(page.getByRole('table', { name: 'Riferimenti risolti — quote sui voti validi', exact: true })).toBeVisible();
  expect(errors).toEqual([]);
  await page.screenshot({ path: 'test-results/reform-editor-preview.png', fullPage: true });
});

test('old candidate policy upgrades explicitly and explains ordinary-only supplied people', async ({ page }) => {
  await open(page);
  const old = structuredClone(defaults); delete old.candidateGeneration.bonusPolicy;
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'old-selection.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(old)) });
  await expect.poll(async () => (await stored(page)).candidateGeneration.bonusPolicy ?? 'original').toBe('original');
  await page.getByText('Candidature e anteprima del premio', { exact: true }).click();
  await page.getByRole('button', { name: 'Aggiorna selezione del premio', exact: true }).click();
  await expect.poll(async () => (await stored(page)).candidateGeneration.bonusPolicy).toBe('reference-proportional-v1');
  await expect(page.getByText('Un candidato ordinario fornito resta escluso dal premio', { exact: false })).toBeVisible();
  await page.reload(); await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  expect((await stored(page)).candidateGeneration.bonusPolicy).toBe('reference-proportional-v1');
});


test('row details control presence and badges while editor scopes stay local', async ({ page }) => {
  await open(page);
  await expect(page.getByLabel('Abilita modifiche avanzate per Camera/Senato')).toHaveCount(0);
  const lead = defaults.lists.camera[0];
  const row = page.getByTestId('reform-list-row').first();
  const settings = row.getByRole('button', { name: `Impostazioni lista ${lead.name}`, exact: true });
  await expect(settings).toHaveAttribute('aria-expanded', 'false');
  await settings.click();
  await row.getByRole('checkbox', { name: `Presenza lista ${lead.name} senato`, exact: true }).uncheck();
  await page.getByRole('button', { name: 'Applica modifica e rimuovi dipendenze' }).click();
  await expect.poll(async () => (await stored(page)).lists.senato.some(l => l.id === lead.id)).toBe(false);
  await expect(settings.locator('.subject-badge')).toBeVisible();
  await row.getByLabel('Nome lista', { exact: true }).fill('Lista Camera');
  await row.getByLabel('Nome lista', { exact: true }).press('Tab');
  expect((await stored(page)).lists.senato.some(l => l.id === lead.id)).toBe(false);
  await row.getByRole('checkbox', { name: 'Presenza lista Lista Camera senato', exact: true }).check();
  await expect(row.locator('.subject-badge')).toHaveCount(0);
  await row.getByRole('checkbox', { name: 'Dichiara tutela', exact: true }).check();
  await expect(row.locator('.subject-badge')).toBeVisible();
  await row.getByLabel('Riferimento tutela').fill('Dichiarazione di prova'); await row.getByLabel('Riferimento tutela').press('Tab');
  await page.getByRole('combobox', { name: 'Ramo riferimenti', exact: true }).selectOption('camera');
  await page.getByText('Depositi delle liste', { exact: true }).click();
  await page.getByRole('combobox', { name: 'Ramo deposito', exact: true }).selectOption('senato');
  await expect(page.getByRole('table', { name: 'Depositi della lista selezionata' }).locator('tbody tr').first()).toContainText('senato');
  await page.getByText('Corrispondenze storiche', { exact: true }).first().click();
  await expect(page.getByRole('combobox', { name: 'Ramo corrispondenze', exact: true })).toHaveValue('common');
  await expect(page.getByRole('combobox', { name: 'Ramo riferimenti', exact: true })).toHaveValue('camera');
});
