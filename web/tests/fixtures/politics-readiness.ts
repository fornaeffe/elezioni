import { expect, test, type Page } from '@playwright/test';

// Bound setup independently from DOM actions. The test budget must cover the
// model wait plus assertions, execution and exports; never shorten the caller's.
export const politicsTestMs = 6 * 60_000;
export const fittingWaitMs = 5 * 60_000;
export function allowPoliticsSetup(minimum = politicsTestMs) {
  if (test.info().timeout < minimum) test.setTimeout(minimum);
}
export async function waitForPoliticsReady(page: Page, refitting = false) {
  allowPoliticsSetup();
  const status = page.locator('aside[aria-label="Stato dell\'applicazione"]');
  // Hidden status details can contain an older ready entry while the active
  // operation is still working. Read only the visible toggle's current label,
  // and keep its readiness observation in one retried assertion.
  await expect.poll(async () => {
    const label = await status.getByRole('button', { name: /^Stato:/ }).getAttribute('aria-label');
    if (/^Stato: (Operazione non riuscita|Preparazione non riuscita|Errore editor)/.test(label ?? ''))
      throw new Error(`Politics preparation failed: ${label}`);
    return label;
  }, { timeout: refitting ? fittingWaitMs : 120_000 }).toMatch(/^Stato: Modello pronto(?:\s|$)/);
}
