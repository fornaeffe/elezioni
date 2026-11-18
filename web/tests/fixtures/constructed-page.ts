import { expect, type Page, type BrowserContext } from '@playwright/test';
import { commonPoliticsFixture } from './common-politics-input';
import { politicsDraftKey } from '../../src/lib/politics/files/draft';
import { REFORM, ROSATELLUM } from '../../src/lib/politics/scenario/schema';
const base = process.env.BASE_PATH ?? '';
export async function openConstructedPolitics(context: BrowserContext, page: Page, law: typeof REFORM | typeof ROSATELLUM, model: 'M01' | 'M02') {
  const f = await commonPoliticsFixture(law, model);
  f.scenario.execution.simulations = 3; f.scenario.execution.workers = { kind: 'fixed', count: 1 };
  await context.route('**/constructed/*.json', route => {
    const id = decodeURIComponent(new URL(route.request().url()).pathname.split('/').at(-1)!.replace(/\.json$/, ''));
    const bytes = f.bytes.get(id);
    return bytes ? route.fulfill({ body: Buffer.from(bytes), contentType: 'application/json' }) : route.abort();
  });
  await context.route('**/data/reform-2026/fast-share/*.gz', route => {
    const name = new URL(route.request().url()).pathname.split('/').at(-1)!;
    const bytes = f.original.files.get(name);
    return bytes ? route.fulfill({ body: Buffer.from(bytes), contentType: 'application/octet-stream' }) : route.continue();
  });
  await page.goto(`${base}/`);
  await page.evaluate(({ key, scenario }) => localStorage.setItem(key, JSON.stringify(scenario)), { key: politicsDraftKey, scenario: f.scenario });
  await page.goto(`${base}/politics`);
  await expect(page.locator('aside[aria-label="Stato dell\'applicazione"]')).toContainText('Modello pronto', { timeout: 60000 });
  return f;
}
