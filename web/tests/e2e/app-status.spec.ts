import { expect, test, type Page } from '@playwright/test';
import { politicsSessionsKey } from '../../src/lib/politics/workspace-controller';
import { installEditorPreparation } from '../fixtures/editor-preparation';

async function holdData(page: Page) {
  let release!: () => void;
  const pending = new Promise<void>(resolve => { release = resolve; });
  await page.route('**/data/**', async route => { await pending; await route.abort().catch(() => {}); });
  return release;
}

test('working status keeps its icon visible on mobile and respects reduced motion', async ({ page }) => {
  const release = await holdData(page);
  try {
    await page.setViewportSize({ width: 390, height: 700 });
    await page.goto(`${process.env.BASE_PATH ?? ''}/`);
    const box = page.locator('aside[aria-label="Stato dell\'applicazione"]');
    const toggle = page.getByRole('button', { name: /^Stato:/ });
    await expect(box).toHaveAttribute('data-state', 'ready');
    await expect(toggle.locator('svg')).toBeVisible();
    await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
    await expect(page.getByLabel('Carica scenario', { exact: true })).toBeAttached();
    await expect(box).toHaveAttribute('data-state', 'working');
    await expect(toggle.locator('.working-icon svg')).toBeVisible();
    await expect(toggle.locator('.toggle-label')).toBeHidden();
    await expect(toggle.locator('.working-icon')).toHaveCSS('animation-duration', '1s');
    await page.emulateMedia({ reducedMotion: 'reduce' });
    await expect(toggle.locator('.working-icon svg')).toBeVisible();
    await expect(toggle.locator('.working-icon')).toHaveCSS('animation-name', 'none');
    await page.setViewportSize({ width: 1000, height: 700 });
    await expect(toggle.locator('.working-icon svg')).toBeVisible();
    await expect(toggle.locator('.toggle-label')).toBeVisible();
  } finally {
    release();
  }
});

test('status remains fixed and compact on mobile, with accessible error details', async ({ page }) => {
  // Exercise the working/error UI with controlled loading, without racing a
  // real model fit. Actual preparation is covered by the lifecycle suites.
  const release = await holdData(page);
  try {
  await page.setViewportSize({ width: 390, height: 700 });
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  const box = page.locator('aside[aria-label="Stato dell\'applicazione"]');
  const toggle = page.getByRole('button', { name: /^Stato:/ });
  await expect(toggle).toBeVisible();
  await expect(toggle.locator(':scope > span').last()).toBeHidden();
  const before = await box.boundingBox();
  await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
  const after = await box.boundingBox();
  expect(Math.abs(before!.y - after!.y)).toBeLessThan(1);
  await page.getByLabel('Carica scenario', { exact: true }).setInputFiles({ name: 'bad.json', mimeType: 'application/json', buffer: Buffer.from('{') });
  await expect(box).toHaveAttribute('data-state', 'error');
  await expect(toggle.locator('svg')).toBeVisible();
  await toggle.click();
  await expect(box.locator('.entry').getByText('Scenario non caricato', { exact: true })).toBeVisible();
  await box.getByRole('button', { name: 'Chiudi avviso' }).click();
  await expect(box).not.toHaveAttribute('data-state', 'error');
  } finally { release(); }
});

test('regional import errors appear in the shared status box', async ({ page }) => {
  await page.goto(`${process.env.BASE_PATH ?? ''}/emilia-romagna`);
  const box = page.locator('aside[aria-label="Stato dell\'applicazione"]');
  await page.locator('input[type=file]').first().setInputFiles({ name: 'bad.json', mimeType: 'application/json', buffer: Buffer.from('{') });
  await expect(box).toHaveAttribute('data-state', 'error');
  await page.getByRole('button', { name: /^Stato:/ }).click();
  await expect(box.locator('strong').filter({ hasText: 'Scenario regionale non caricato' })).toBeVisible();
});

test('the status box presents a structured worker error with actionable guidance', async ({ page }) => {
  const diagnostic = { code: 'REFERENCE_SUBJECT', name: 'Error', message: 'component references require a declared new component', stack: 'private stack' };
  await page.route('**/politics-workspace.worker-*.js', route => route.fulfill({
    contentType: 'text/javascript',
    body: `self.onmessage = ({ data: { type, requestId, action } }) => {
      if (action === 'prepare') self.postMessage({ type, requestId, ok: false, error: ${JSON.stringify(diagnostic)} });
    };`,
  }));

  const release = await holdData(page);
  try {
    await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
    const box = page.locator('aside[aria-label="Stato dell\'applicazione"]');
    await expect(box.getByRole('status')).toHaveText(/Impossibile preparare il modello.*Controlla che i riferimenti/);
    await box.getByRole('button', { name: 'Stato: Impossibile preparare il modello' }).click();
    const entry = box.locator('.entry').filter({ hasText: diagnostic.code });
    await expect(entry.getByText(/Controlla che i riferimenti/)).toBeVisible();
    await expect(entry.getByText(diagnostic.code, { exact: true })).toBeVisible();
    await expect(entry).toContainText(diagnostic.message);
    await expect(entry).not.toContainText(/Error:|private stack/);
  } finally { release(); }
});

test('an unavailable local result keeps its diagnostic code and its operation label', async ({ page }) => {
  await installEditorPreparation(page);
  await page.addInitScript(key => {
    localStorage.setItem(key, JSON.stringify({
      owner: 'test-owner', selected: 'missing', sessions: [{
        id: 'missing', law: 'politiche-reform-2026-v1', model: 'M02@3.0.0', createdAt: '2026-11-27T00:00:00Z',
        renewal: 'joint', completed: 0, requested: 1,
      }],
    }));
  }, politicsSessionsKey);
  const release = await holdData(page);
  try {
    await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
    const box = page.locator('aside[aria-label="Stato dell\'applicazione"]');
    await box.getByRole('button', { name: 'Stato: Sessione selezionata non disponibile' }).click();
    const entry = box.locator('.entry').filter({ hasText: 'RESULT_SERVICE_ERROR' });
    await expect(entry.getByText('RESULT_SERVICE_ERROR', { exact: true })).toBeVisible();
    await expect(entry).toContainText('Local result session is unavailable');
  } finally { release(); }
});

