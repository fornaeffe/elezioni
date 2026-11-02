import { test, expect, type Page } from '@playwright/test';
import { mkdir, writeFile, stat } from 'node:fs/promises';
import { resolve } from 'node:path';
import { build } from 'vite';
import { watchMemory } from '../fixtures/fast-share-memory';
import { createReadStream } from 'node:fs';
import { createGunzip } from 'node:zlib';
import { createHash } from 'node:crypto';

/** Compare every decompressed byte except the export timestamp, with bounded RAM. */
async function archiveDigest(path: string) {
  const hash = createHash('sha256');
  let prefix = Buffer.alloc(0), header = false, bytes = 0;
  for await (const part of createReadStream(path).pipe(createGunzip())) {
    const chunk = Buffer.from(part); bytes += chunk.length;
    if (header) hash.update(chunk);
    else {
      prefix = Buffer.concat([prefix, chunk]);
      if (prefix.length < 256) continue;
      const text = prefix.toString('latin1');
      const normalized = text.replace(/^(\{"schema_version":\d+,"exportedAt":)"[^"]+"/, '$1"fixed"');
      if (normalized === text) throw new Error('Missing archive export timestamp');
      hash.update(Buffer.from(normalized, 'latin1')); header = true; prefix = Buffer.alloc(0);
    }
  }
  if (!header) throw new Error('Truncated result archive');
  return { bytes, sha256: hash.digest('hex') };
}
const base = process.env.BASE_PATH ?? '',
  rootSmoke = process.env.ACCEPTANCE_ROOT_SMOKE === '1',
  draws = Number(process.env.FV08_DRAWS ?? (process.env.FV08_FULL === '1' ? 1000 : 3)),
  directory = resolve('../data/derived/fv08-unified/browser'),
  bundles = new Map<string, string>();
test.beforeAll(async () => {
  await mkdir(directory, { recursive: true });
  const result = await build({
    configFile: false,
    logLevel: 'error',
    build: {
      write: false,
      minify: false,
      lib: {
        entry: resolve('tests/fixtures/unified-acceptance-harness.ts'),
        formats: ['es'],
        fileName: () => 'fv08-inspect.js',
      },
    },
  });
  for (const r of Array.isArray(result) ? result : [result])
    if ('output' in r)
      for (const out of r.output)
        if (out.type === 'chunk')
          bundles.set(base + '/' + out.fileName, out.code);
});
async function inspect(page: Page) {
  return page.evaluate(async (root) => {
    const h = await import(/* @vite-ignore */ root + '/fv08-inspect.js'),
      key = 'elezioni:politics-reform-2026:fast-results:v1',
      list = JSON.parse(sessionStorage.getItem(key) ?? '[]');
    return h.inspect(list.at(-1).id, sessionStorage.getItem(key + ':owner'));
  }, base);
}
for (const model of ['M02', 'M01'] as const)
  test(model + ' unified browser workflow', async ({ page, browser }) => {
    await page.route('**/fv08-inspect.js', async (route) => {
      const code = bundles.get(new URL(route.request().url()).pathname);
      if (!code) throw new Error('Missing verification harness');
      await route.fulfill({
        contentType: 'application/javascript',
        body: code,
      });
    });
    // Functional acceptance does not need resource instrumentation. Dedicated
    // full campaigns retain heap/process sampling and their existing receipts.
    const memory = process.env.FV08_FULL === '1' ? await watchMemory(browser, page) : undefined,
      pageErrors: string[] = [];
    page.on('pageerror', (e) => pageErrors.push(e.message));
    await page.goto(base + '/politics');
    await page
      .getByLabel('Algoritmo di scrutinio')
      .selectOption('politiche-reform-2026-v1');
    await expect(
      page.getByLabel('Modello di generazione', { exact: true }),
    ).toHaveValue('M02');
    await page
      .getByRole('button', { name: 'Carica modello predefinito', exact: true })
      .click();
    await expect(page.getByTestId('fit-status')).toHaveText('Modello pronto', {
      timeout: 60000,
    });
    await page
      .getByLabel('Modello di generazione', { exact: true })
      .selectOption(model);
    await page
      .getByRole('button', { name: 'Verifica scenario', exact: true })
      .click();
    await expect(page.getByTestId('fit-status')).toHaveText('Modello pronto', {
      timeout: 60000,
    });
    if (process.env.FV08_WORKERS)
      await page.getByLabel('Processi di calcolo', { exact: true })
        .selectOption(process.env.FV08_WORKERS);
    const workers = Number(
      await page
        .getByLabel('Processi di calcolo', { exact: true })
        .inputValue(),
    );
    const deviceHints = await page.evaluate(() => ({
      hardwareConcurrency: navigator.hardwareConcurrency,
      deviceMemory: (navigator as Navigator & {deviceMemory?:number}).deviceMemory,
    }));
    if (draws === 3)
      await page
        .getByLabel('Processi di calcolo', { exact: true })
        .selectOption('2');
    await page.getByLabel('Simulazioni', { exact: true }).fill(String(draws));
    const started = Date.now();
    await page
      .getByRole('button', { name: 'Esegui riforma', exact: true })
      .click();
    const crashed = new Promise<never>((_, reject) =>
      page.once('crash', () => reject(new Error('Browser renderer crashed during campaign'))),
    );
    const progressTimer = draws === 1000 ? setInterval(() => {
      void page.getByTestId('reform-phase').textContent({timeout:5000})
        .then(phase => console.log(model + ': ' + phase)).catch(() => undefined);
    }, 30000) : undefined;
    try {
      await Promise.race([
        expect(page.getByTestId('reform-phase')).toHaveText('completed', {
          timeout: 60 * 60 * 1000,
        }),
        crashed,
      ]);
    } finally { clearInterval(progressTimer); }
    const elapsedMs = Date.now() - started;
    await expect(page.getByTestId('stored-result-status')).toContainText(
      draws + '/' + draws,
    );
    const before = await inspect(page);
    const receiptPath = resolve(
      directory,
      browser.browserType().name() +
        '-' +
        model +
        (base ? '-nested' : '-root') +
        '-' +
        draws +
        '.json.gz.receipt.json',
    );
    await writeFile(
      receiptPath,
      JSON.stringify(
        { phase: 'computed', model, base, draws, elapsedMs, ...before },
        null,
        2,
      ) + '\n',
    );
    expect(before.runs).toHaveLength(draws);
    expect(
      before.runs.map((r: { simulation: number }) => r.simulation),
    ).toEqual(Array.from({ length: draws }, (_, i) => i + 1));
    expect(before.fitFilePresent).toBe(true);
    expect(before).toMatchObject({model,modelVersion:"2.0.0"});
    for (const run of before.runs)
      expect(run).toMatchObject({
        status: 'complete',
        seats: 588,
        distinctCandidates: 588,
        camera: 392,
        senato: 196,
        unfilled: 0,
      });
    expect(before.lastRunDetail.rows).toHaveLength(10);
    expect(before.lastRunDetail.rows.every((r: { Simulation: number }) => r.Simulation === draws)).toBe(true);
    if (rootSmoke) {
      expect(base).toBe('');
      expect(draws).toBe(1);
      expect(pageErrors).toEqual([]);
      return;
    }
    await expect(
      page.getByRole('heading', {
        name: 'Eletti medi e percentuali sui voti validi',
        exact: true,
      }),
    ).toBeVisible({ timeout: 60000 });
    const archive = resolve(
      directory,
      browser.browserType().name() +
        '-' +
        model +
        (base ? '-nested' : '-root') +
        '-' +
        draws +
        '.json.gz',
    );
    const download = page.waitForEvent('download', { timeout: 20 * 60 * 1000 });
    await page
      .getByRole('button', { name: 'Scarica risultati JSON.gz', exact: true })
      .click();
    await (await download).saveAs(archive);
    await page.getByTestId('reform-file-input').setInputFiles(archive);
    await page.waitForFunction(
      (previous) => {
        const list = JSON.parse(
          sessionStorage.getItem(
            'elezioni:politics-reform-2026:fast-results:v1',
          ) ?? '[]',
        );
        return list.at(-1)?.id !== previous;
      },
      before.storage.id,
      { timeout: 20 * 60 * 1000 },
    );
    await expect(
      page.getByText('Caricamento file…', { exact: true }),
    ).toHaveCount(0, { timeout: 20 * 60 * 1000 });
    const after = await inspect(page);
    expect(after.runs).toEqual(before.runs);
    expect(after.seatsByList).toEqual(before.seatsByList);
    expect(after.lastRunDetail).toEqual(before.lastRunDetail);
    const roundtrip = archive + '.roundtrip.gz';
    const reexport = page.waitForEvent('download', { timeout: 20 * 60 * 1000 });
    await page.getByRole('button', { name: 'Scarica risultati JSON.gz', exact: true }).click();
    await (await reexport).saveAs(roundtrip);
    const archiveContent = await archiveDigest(archive);
    expect(await archiveDigest(roundtrip)).toEqual(archiveContent);
    await page
      .getByRole('button', {
        name: 'Ripristina scenario dei risultati',
        exact: true,
      })
      .click();
    await expect(page.getByTestId('fit-status')).toHaveText('Modello pronto', {
      timeout: 60000,
    });
    await expect(
      page.getByLabel('Modello di generazione', { exact: true }),
    ).toHaveValue(model);
    if (draws === 3)
      await expect(
        page.getByLabel('Processi di calcolo', { exact: true }),
      ).toHaveValue('2');
    const memoryReport = await memory?.stop() ?? { supported: false, reason: 'Functional acceptance; use FV08_FULL=1 for resource sampling' };
    await writeFile(
      archive + '.receipt.json',
      JSON.stringify(
        {
          browser: browser.browserType().name(),
          browserVersion: browser.version(),
          model,
          base,
          draws,
          workers: draws === 3 ? 2 : workers,
          deviceHints,
          concurrentModelCampaigns: process.env.FV08_FULL === '1' ? Number(process.env.FV08_CAMPAIGN_WORKERS ?? 1) : 1,
          elapsedMs,
          archiveBytes: (await stat(archive)).size,
          archiveContent,
          ...before,
          memory: memoryReport,
          pageErrors,
        },
        null,
        2,
      ) + '\n',
    );
    expect(pageErrors).toEqual([]);
    expect(elapsedMs).toBeLessThan(60 * 60 * 1000);
  });

if (draws === 3) {
  test('abundant device RAM still respects browser memory headroom without hiding CPU choices', async ({page}) => {
    await page.addInitScript(() => {
      Object.defineProperty(navigator, 'hardwareConcurrency', {get:()=>28});
      Object.defineProperty(navigator, 'deviceMemory', {get:()=>64});
    });
    await page.goto(base + '/politics');
    await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
    await page.getByRole('button',{name:'Carica modello predefinito',exact:true}).click();
    await expect(page.getByTestId('fit-status')).toHaveText('Modello pronto',{timeout:60000});
    const workers=page.getByLabel('Processi di calcolo',{exact:true});
    await expect(workers).toHaveValue('10');
    await expect(workers.locator('option')).toHaveCount(26);
    await workers.selectOption('26');
    await expect(page.getByText(/supera il margine prudenziale di memoria del browser/)).toBeVisible();
    await expect(page.getByText(/Attenzione: il numero scelto/)).toHaveCount(0);
    await page.getByLabel('Modello di generazione',{exact:true}).selectOption('M01');
    await expect(workers).toHaveValue('10');
  });
  test('composition editing preserves entered ratios, offers equal allocation and leaves candidate SDs inactive in M01', async ({
    page,
  }) => {
    await page.goto(base + '/politics');
    await page
      .getByLabel('Algoritmo di scrutinio')
      .selectOption('politiche-reform-2026-v1');
    await page
      .getByRole('button', { name: 'Carica modello predefinito', exact: true })
      .click();
    await expect(page.getByTestId('fit-status')).toHaveText('Modello pronto', {
      timeout: 60000,
    });
    await page
      .getByText('Dati avanzati: sondaggi, corrispondenze e data', {
        exact: true,
      })
      .click();
    await page
      .getByText('Corrispondenze storiche indivisibili', { exact: true })
      .click();
    const history = page
      .getByText('Corrispondenze storiche indivisibili', { exact: true })
      .locator('..');
    await history
      .getByRole('combobox', { name: 'Elezione', exact: true })
      .selectOption('camera-2022');
    await page
      .getByLabel('Cerca osservazioni', { exact: true })
      .fill('AZIONE - ITALIA VIVA - CALENDA');
    await history
      .getByRole('button', {
        name: 'AZIONE - ITALIA VIVA - CALENDA',
        exact: true,
      })
      .click();
    await history
      .getByText('Composizione assunta per M01', {
        exact: true,
      })
      .click();
    await history
      .getByLabel('azione — peso relativo', { exact: true })
      .fill('60');
    await history
      .getByLabel('azione — peso relativo', { exact: true })
      .press('Tab');
    await history
      .getByLabel('italia-viva — peso relativo', { exact: true })
      .fill('40');
    await history
      .getByLabel('italia-viva — peso relativo', { exact: true })
      .press('Tab');
    await expect(
      history.getByLabel('azione — peso relativo', { exact: true }),
    ).toHaveValue('60');
    await expect(
      history.getByText('Quota normalizzata: 60%', { exact: true }),
    ).toBeVisible();
    await history
      .getByRole('button', {
        name: 'Distribuisci in parti uguali',
        exact: true,
      })
      .click();
    await expect(
      history.getByLabel('azione — peso relativo', { exact: true }),
    ).toHaveValue('0.5');
    await page
      .getByLabel('Modello di generazione', { exact: true })
      .selectOption('M01');
    await expect(page.getByTestId('fit-status')).toHaveText('Modello pronto', {
      timeout: 60000,
    });
    await page
      .getByText('Quote di riferimento e incertezza', { exact: true })
      .click();
    await expect(
      page.getByLabel('Deviazione standard (punti %)', { exact: true }),
    ).toBeDisabled();
    await expect(page.getByLabel('Seme della stima',{exact:true})).toBeDisabled();
  });
  test('worker defaults use half declared RAM, CPU choices remain available, and excessive memory is warned', async ({
    page,
  }) => {
    await page.addInitScript(() => {
      Object.defineProperty(navigator, 'hardwareConcurrency', { get: () => 8 });
      Object.defineProperty(navigator, 'deviceMemory', { get: () => 2 });
    });
    await page.goto(base + '/politics');
    await page
      .getByLabel('Algoritmo di scrutinio')
      .selectOption('politiche-reform-2026-v1');
    await page
      .getByRole('button', { name: 'Carica modello predefinito', exact: true })
      .click();
    await expect(page.getByTestId('fit-status')).toHaveText('Modello pronto', {
      timeout: 60000,
    });
    const workers = page.getByLabel('Processi di calcolo', { exact: true });
    await expect(workers).toHaveValue('2');
    await expect(workers.locator('option')).toHaveCount(6);
    await workers.selectOption('6');
    await expect(page.getByText(/Attenzione: il numero scelto/)).toBeVisible();
    await workers.selectOption('1');
    await page.getByLabel('Simulazioni', { exact: true }).fill('1');
    await page
      .getByRole('button', { name: 'Esegui riforma', exact: true })
      .click();
    await expect(page.getByTestId('reform-phase')).toHaveText('completed', {
      timeout: 120000,
    });
  });
}
