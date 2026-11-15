import { expect, type Page } from '@playwright/test';
import { readFile } from 'node:fs/promises';
import { createDefaultCommonPoliticsScenario } from '../../src/lib/politics/scenario/defaults';
import { politicsDraftKey } from '../../src/lib/politics/files/draft';
import { politicsSessionsKey } from '../../src/lib/politics/workspace-controller';
import { type LawId, type GeneratorId, type PoliticsScenario, REFORM } from '../../src/lib/politics/scenario/schema';
export const base = process.env.BASE_PATH ?? '';
export async function openCommon(page: Page, law: LawId = REFORM, model: GeneratorId = 'M02', simulations = 1) {
    const initial = createDefaultCommonPoliticsScenario();
    const scenario: PoliticsScenario = { ...initial, selection: { ...initial.selection, scrutiny: { ...initial.selection.scrutiny, id: law },
            generatorsByLaw: { ...initial.selection.generatorsByLaw, [law]: { id: model, version: '2.0.0' } } },
        execution: { ...initial.execution, simulations, workers: { kind: 'fixed', count: 2 } } };
    await page.goto(base + '/');
    await page.evaluate(({ key, scenario }) => localStorage.setItem(key, JSON.stringify(scenario)), { key: politicsDraftKey, scenario });
    await page.goto(base + '/politics');
    await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]')).toContainText('Modello pronto', { timeout: 120000 });
    return scenario;
}
export async function runCommon(page: Page) {
    await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]')).toContainText(/Modello pronto|Preparazione non riuscita/, { timeout: 120000 });
    expect(await page.locator('aside[aria-label="Stato dell\'applicazione"]').innerText(), (await page.getByRole('alert').allTextContents()).join('\n')).toContain('Modello pronto');
    await page.getByRole('button', { name: 'Avvia simulazione', exact: true }).click();
    await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]')).toContainText(/Esecuzione completata|Esecuzione interrotta|Esecuzione non riuscita/, { timeout: 120000 });
    expect(await page.locator('aside[aria-label="Stato dell\'applicazione"]').innerText(), (await page.getByRole('alert').allTextContents()).join('\n')).toContain('Esecuzione completata');
    return selectedCommon(page);
}
export async function openResultTransfers(page: Page) {
    const summary = page.getByText('Caricamento, selezione e download dei risultati', { exact: true });
    if (!await summary.evaluate(node => (node.parentElement as HTMLDetailsElement).open)) await summary.click();
}
export async function selectedCommon(page: Page) {
    return page.evaluate(async (key) => {
        const index = JSON.parse(localStorage.getItem(key)!);
        const db = await new Promise<IDBDatabase>((resolve, reject) => { const r = indexedDB.open('elezioni-result-chunks-v1'); r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); });
        try {
            return await new Promise<any>((resolve, reject) => { const r = db.transaction('results').objectStore('results').get(index.selected); r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); });
        }
        finally {
            db.close();
        }
    }, politicsSessionsKey);
}
export async function downloadCommon(page: Page, name: string) {
    await openResultTransfers(page);
    const pending = page.waitForEvent('download', { timeout: 120000 });
    await page.getByRole('button', { name, exact: true }).click();
    return readFile((await (await pending).path())!);
}
export async function expectStatusError(page: Page, message: string | RegExp) {
    const box = page.locator('aside[aria-label="Stato dell\'applicazione"]');
    await expect(box).toHaveClass(/error/);
    const toggle = box.getByRole('button', { name: /^Stato:/ });
    if (await toggle.getAttribute('aria-expanded') !== 'true') await toggle.click();
    await expect(box.getByText(message)).toBeVisible();
}
