import { expect, test, type Page } from '@playwright/test';

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
  await expect(box.getByText(/Scenario non caricato/)).toBeVisible();
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
  await expect(box.locator('strong').filter({ hasText: 'Operazione regionale non riuscita' })).toBeVisible();
});

