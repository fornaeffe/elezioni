import { expect, test, type Page } from '@playwright/test';

// Bound setup independently from DOM actions. The test budget must cover the
// model wait plus assertions, execution and exports; never shorten the caller's.
export const politicsTestMs = 6 * 60_000;
export const fittingWaitMs = 5 * 60_000;
export const productionWorkflowMs = 10 * 60_000;
export function allowPoliticsSetup(minimum = politicsTestMs) {
  if (test.info().annotations.some(a => a.type === 'editor-preparation')) return;
  if (test.info().timeout < minimum) test.setTimeout(minimum);
}
export async function waitForPoliticsReady(page: Page, _refitting = false) {
  allowPoliticsSetup();
  const status = page.locator('aside[aria-label="Stato dell\'applicazione"]');
  // Hidden status details can contain an older ready entry while the active
  // operation is still working. Read only the visible toggle's current label,
  // and keep its readiness observation in one retried assertion.
  let lastProgress = '';
  try { await expect.poll(async () => {
    const label = await status.getByRole('button', { name: /^Stato:/ }).getAttribute('aria-label');
    if (/^Stato: (Operazione non riuscita|Preparazione non riuscita|Errore editor)/.test(label ?? ''))
      throw new Error(`Politics preparation failed: ${label}`);
    lastProgress = ((await status.textContent()) ?? '').slice(-1500);
    return label;
  }, { timeout: test.info().annotations.some(a => a.type === 'editor-preparation') ? 20_000 : fittingWaitMs }).toMatch(/^Stato: Modello pronto(?:\s|$)/);
  } catch (error) {
    throw new Error(`Model readiness failed; last status/progress: ${lastProgress}`, { cause: error });
  }
}
