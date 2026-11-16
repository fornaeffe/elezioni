/** Run against the staged Pages tree, then the actual deployed main revision. */
import { expect, test } from '@playwright/test';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { openCommon, runCommon, downloadCommon } from '../fixtures/common-page';
import { ROSATELLUM, REFORM } from '../../src/lib/politics/scenario/schema';
const base = process.env.BASE_PATH ?? '/elezioni';

test.beforeEach(async ({ context, page }, info) => {
  const requests: { url: string; status: number; encodedBodyBytes: number; headersBytes: number; contentEncoding: string | null }[] = [];
  const pending: Promise<void>[] = [], errors: string[] = [];
  page.on('pageerror', e => errors.push(e.message));
  context.on('requestfailed', r => {
    const reason = r.failure()?.errorText;
    // Direct-load/refresh checks deliberately cancel the preceding document.
    if (reason !== 'net::ERR_ABORTED' && reason !== 'NS_BINDING_ABORTED') errors.push(`${r.url()}: ${reason}`);
  });
  context.on('response', response => {
    pending.push((async () => {
      if (new URL(response.url()).pathname.startsWith(base + '/')) {
        if (await response.finished()) return; // Navigation may cancel an earlier document.
        const sizes = await response.request().sizes();
        requests.push({ url: response.url(), status: response.status(), encodedBodyBytes: sizes.responseBodySize,
          headersBytes: sizes.responseHeadersSize, contentEncoding: response.headers()['content-encoding'] ?? null });
      } else errors.push(`Request outside project base: ${response.url()}`);
    })().catch(error => { errors.push(`Transfer measurement: ${error}`); }));
  });
  // A fresh browser context per test measures a cold browser visit; export
  // bytes are generated locally and are not counted as hosting bandwidth.
  info.annotations.push({ type: 'hosting', description: 'Fresh browser context; encoded response bytes measured by Playwright' });
  const finish = async () => {
    await Promise.all(pending);
    const output = resolve(process.env.HOSTING_TRANSFER_OUTPUT ?? '../data/derived/hosting/public-transfers');
    await mkdir(output, { recursive: true });
    const bodyBytes = requests.reduce((n, r) => n + r.encodedBodyBytes, 0);
    await writeFile(resolve(output, `${info.title.replace(/[^a-z0-9]+/gi, '-')}.json`), JSON.stringify({
      test: info.title, url: process.env.HOSTING_URL ?? 'local staged site', requests,
      encodedBodyBytes: bodyBytes,
      trafficUnit: info.title.includes('direct routes') ? 'synthetic multi-route refresh matrix' : 'cold-browser election workflow',
      approximateWorkflowsPer100GB: bodyBytes ? Math.floor(100_000_000_000 / bodyBytes) : null,
      errors,
    }, null, 2));
    expect(errors).toEqual([]);
    expect(requests.filter(r => r.status >= 400)).toEqual([]);
  };
  (info as typeof info & { hostingFinish: () => Promise<void> }).hostingFinish = finish;
});
test.afterEach(async ({}, info) => {
  await (info as typeof info & { hostingFinish: () => Promise<void> }).hostingFinish();
});

test('Pages direct routes refresh navigation and revision', async ({ page, request }) => {
  const release = await request.get(`${base}/release.json`);
  expect(release.status()).toBe(200);
  const metadata = await release.json();
  expect(metadata.base).toBe(base);
  if (process.env.HOSTING_EXPECTED_REVISION) expect(metadata.revision).toBe(process.env.HOSTING_EXPECTED_REVISION);
  for (const route of ['/', '/politics/', '/politics/editor/', '/emilia-romagna/']) {
    expect((await page.goto(base + route))?.status()).toBe(200);
    await expect(page.locator('h1')).toBeVisible();
    expect((await page.reload())?.status()).toBe(200);
    await expect(page.locator('h1')).toBeVisible();
  }
  await page.goto(base + '/');
  await page.getByRole('link', { name: /politiche/i }).click();
  await expect(page.getByLabel('Legge elettorale')).toBeVisible();
  await page.goBack();
  await page.getByRole('link', { name: /emilia-romagna/i }).click();
  await expect(page.getByRole('heading', { name: /Emilia-Romagna/ })).toBeVisible();
});

for (const law of [ROSATELLUM, REFORM] as const) for (const model of ['M01', 'M02'] as const) {
  test(`Pages production workers and exports ${law} ${model}`, async ({ page }) => {
    await openCommon(page, law, model, 1);
    const stored = await runCommon(page);
    expect(stored.result.completedSimulations).toBe(1);
    expect(stored.politics.run.session.simulations).toEqual([{ id: 1, outcome: 'complete' }]);
    const archive = await downloadCommon(page, 'Scarica archivio risultati JSON.gz');
    const exported = JSON.parse(gunzipSync(archive).toString());
    expect(exported.kind).toBe('politics-result-archive');
    expect(exported.run.executedScenario.selection.scrutiny.id).toBe(law);
    expect(exported.run.execution.generator.id).toBe(model);
    expect(exported.run.result.completedSimulations).toBe(1);
    expect((await downloadCommon(page, 'Scarica risultati CSV')).length).toBeGreaterThan(100);
  });
}

test('Pages regional worker and structured CSV exports', async ({ page }) => {
  await page.goto(base + '/emilia-romagna/');
  await page.getByLabel('Simulazioni').fill('1');
  await page.getByRole('button', { name: 'Esegui', exact: true }).click();
  await expect(page.getByRole('table', { name: 'Regional election overview' })).toBeVisible({ timeout: 60_000 });
  for (const label of ['Scarica risultati compressi', 'Scarica risultati CSV']) {
    const pending = page.waitForEvent('download');
    await page.getByRole('button', { name: label, exact: true }).click();
    const bytes = await readFile((await (await pending).path())!);
    expect(bytes.length).toBeGreaterThan(100);
    if (label.endsWith('compressi')) {
      const result = JSON.parse(gunzipSync(bytes).toString());
      expect(result.scenario).toBeTruthy();
      expect(result.result.status).toBe('completed');
      expect(result.result.benchmark.simulations).toBe(1);
    }
  }
});
