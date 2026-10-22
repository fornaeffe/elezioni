import { createHash } from 'node:crypto';
import { createReadStream, existsSync, mkdirSync, writeFileSync, mkdtempSync, readFileSync, statSync } from 'node:fs';
import { resolve, join, basename, sep } from 'node:path';
import { tmpdir } from 'node:os';
import { rm } from 'node:fs/promises';
import { createGunzip } from 'node:zlib';
import { chromium, expect, test } from '@playwright/test';
import catalog from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json' with { type: 'json' };

async function archiveDigest(path: string) {
  const hash = createHash('sha256'); let bytes = 0, first = true;
  for await (const part of createReadStream(path).pipe(createGunzip())) {
    let chunk = Buffer.from(part); bytes += chunk.length;
    // Byte-preserving replacement even when the first buffer ends inside UTF-8.
    if (first) { chunk = Buffer.from(chunk.toString('latin1').replace(/"exportedAt":"[^"]+"/, '"exportedAt":"fixed"'), 'latin1'); first = false; }
    hash.update(chunk);
  }
  return { bytes, sha256: hash.digest('hex') };
}

test('1000 production reform simulations: bounded memory, detail queries, complete gzip roundtrip', async ({}, testInfo) => {
  const profile = mkdtempSync(join(tmpdir(), 'elezioni-result-acceptance-'));
  const context = await chromium.launchPersistentContext(profile, { headless: true, baseURL: 'http://127.0.0.1:4173', viewport: { width: 1280, height: 720 } });
  const page = context.pages()[0] ?? await context.newPage();
  try {
  expect(existsSync(resolve('static', catalog.path)), 'Required production runtime bundle').toBe(true);
  const cdp = await context.newCDPSession(page);
  let workerSession = '', rpc = 0;
  const replies = new Map<number, (value: unknown) => void>();
  cdp.on('Target.attachedToTarget', event => { if (event.targetInfo.type === 'worker') workerSession = event.sessionId; });
  cdp.on('Target.receivedMessageFromTarget', event => {
    const message = JSON.parse(event.message);
    if (message.id) { replies.get(message.id)?.(message.result ?? { error: message.error }); replies.delete(message.id); }
  });
  await cdp.send('Target.setAutoAttach', { autoAttach: true, waitForDebuggerOnStart: false, flatten: false });
  const workerCall = async (method: string) => {
    if (!workerSession) return null;
    const id = ++rpc;
    const result = new Promise<unknown>(resolve => replies.set(id, resolve));
    await cdp.send('Target.sendMessageToTarget', { sessionId: workerSession, message: JSON.stringify({ id, method }) });
    return Promise.race([result, new Promise(resolve => setTimeout(() => { replies.delete(id); resolve(null); }, 5000))]);
  };
  const output = resolve('test-results/reform-1000'); mkdirSync(output, { recursive: true });
  const previous = process.env.RESULT_REPORT ? JSON.parse(readFileSync(process.env.RESULT_REPORT, 'utf8')) : undefined;
  const samples: unknown[] = previous?.samples ?? [], errors: string[] = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto('/politics');
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-reform-2026-v1');
  await page.getByLabel('Simulazioni', { exact: true }).fill('1000');
  const started = Date.now();
  if (process.env.RESULT_ARCHIVE) {
    await page.getByTestId('reform-file-input').setInputFiles(process.env.RESULT_ARCHIVE);
    await expect(page.getByTestId('reform-phase')).toHaveText('imported', { timeout: 15 * 60 * 1000 });
  } else await page.getByRole('button', { name: 'Esegui riforma', exact: true }).click();
  let lastBucket = -1, lastProgress = -1;
  while (!process.env.RESULT_ARCHIVE) {
    const phase = await page.getByTestId('reform-phase').innerText();
    if (phase === 'completed') break;
    expect(phase, await page.locator('body').innerText()).not.toMatch(/not_implemented|^error$/);
    const done = Number(phase.match(/ (\d+)\/1000/)?.[1] ?? 0), bucket = Math.floor(done / 100);
    if (Math.floor(done / 25) > lastProgress) { lastProgress = Math.floor(done / 25); console.log('RESULT_PROGRESS', phase, ((Date.now() - started) / 1000).toFixed(1)); }
    if (bucket > lastBucket && done > 0) {
      lastBucket = bucket;
      await cdp.send('HeapProfiler.collectGarbage'); await workerCall('HeapProfiler.collectGarbage');
      const sample = { completed: done, elapsedSeconds: (Date.now() - started) / 1000, main: await cdp.send('Runtime.getHeapUsage'), worker: await workerCall('Runtime.getHeapUsage') };
      samples.push(sample); console.log('RESULT_MEMORY', JSON.stringify(sample));
      writeFileSync(resolve(output, 'checkpoint.json'), JSON.stringify({ samples }, null, 2));
    }
    await page.waitForTimeout(1000);
  }
  const savedElapsed = process.env.RESULT_ARCHIVE ? Number((await page.getByLabel('Risultati riforma', { exact: true }).innerText()).match(/simulazioni richieste · (\d+) ms/)?.[1]) / 1000 : undefined;
  const computationSeconds = previous?.computationSeconds ?? savedElapsed ?? (Date.now() - started) / 1000;
  writeFileSync(resolve(output, 'checkpoint.json'), JSON.stringify({ computationSeconds, samples }, null, 2));
  await expect(page.getByTestId('stored-result-status')).toContainText('1000/1000');
  await expect(page.getByRole('table', { name: 'Reform overview' })).toContainText('1000');
  const messages = page.getByRole('region', { name: 'Messaggi delle simulazioni', exact: true });
  const messageTable = messages.getByRole('table');
  await expect(messageTable).toBeVisible({ timeout: 120000 });
  const messageSummaryText = await messageTable.innerText();
  const diagnostic = messages.getByRole('button', { name: /^Simulazioni per / }).first();
  let affectedCount: number | null = null;
  if (await diagnostic.count()) {
    affectedCount = Number((await diagnostic.innerText()).match(/^(\d+)\/1000/)?.[1]);
    expect(affectedCount).toBeGreaterThan(0);
    await diagnostic.click();
    const affected = page.getByRole('region', { name: 'Simulazioni interessate', exact: true });
    await expect(affected.locator('.ids li')).toHaveCount(Math.min(50, affectedCount), { timeout: 120000 });
    const idsDownload = page.waitForEvent('download', { timeout: 120000 });
    await affected.getByRole('button', { name: 'Scarica tutti gli ID filtrati CSV', exact: true }).click();
    const idsStream = await (await idsDownload).createReadStream(); let idsCsv = '';
    for await (const chunk of idsStream!) idsCsv += chunk.toString();
    const messageIds = idsCsv.trim().split('\r\n').slice(1).map(Number);
    expect(messageIds).toHaveLength(affectedCount);
    expect(messageIds.every((id, i) => id >= 1 && id <= 1000 && (i === 0 || id > messageIds[i - 1]))).toBe(true);
  } else {
    expect(!!process.env.RESULT_ARCHIVE, 'New production results must provide exact diagnostics').toBe(true);
    await expect(messages).toContainText('Frequenze e ID esatti non ricostruibili');
  }
  const tableOptions = await page.getByLabel('Tabella riforma').locator('option').allTextContents();
  const recovery = tableOptions.find(t => t.startsWith('Candidate recovery traces'))!;
  await page.getByLabel('Tabella riforma').selectOption({ label: recovery });
  const detail = page.locator('.table-scroll > table');
  await expect(detail.locator('tbody tr')).toHaveCount(50);
  const firstRow = await detail.locator('tbody tr').first().innerText();
  await page.getByRole('button', { name: 'Pagina successiva', exact: true }).click();
  await expect(page.getByText(/Pagina 2 di/)).toBeVisible();
  await expect.poll(() => detail.locator('tbody tr').first().innerText()).not.toBe(firstRow);
  await page.getByLabel('Simulazione grafico').fill('1000');
  await page.getByLabel('Ramo grafico').selectOption('senato');
  await expect(page.getByRole('figure')).toContainText('1000, senato');
  const chartStarted = Date.now();
  const statistics = page.getByLabel('Statistiche riforma', { exact: true });
  await expect(statistics.getByRole('heading', { name: 'Eletti medi e percentuali sui voti validi' })).toBeVisible();
  await expect(statistics.getByRole('region', { name: 'Percentuali simulate per lista', exact: true })).toBeVisible();
  const scatter = statistics.getByRole('region', { name: 'Eletti e percentuali per lista', exact: true });
  await scatter.locator(':scope > summary').click();
  const firstCanvas = scatter.locator('canvas').first();
  await expect(firstCanvas).toBeAttached({ timeout: 120000 }); await firstCanvas.scrollIntoViewIfNeeded();
  await expect(firstCanvas).toHaveAttribute('data-complete', 'true', { timeout: 120000 });
  const scatterKnownPoints = Number(await firstCanvas.getAttribute('data-points'));
  expect(scatterKnownPoints).toBeGreaterThan(0); expect(scatterKnownPoints).toBeLessThanOrEqual(1000);
  await scatter.locator(':scope > summary').click(); await expect(scatter.locator('canvas')).toHaveCount(0);
  const spine = statistics.getByRole('region', { name: 'Spinogrammi degli eletti per lista', exact: true });
  await spine.locator(':scope > summary').click();
  const firstSpine = spine.getByRole('button').first(); await expect(firstSpine).toBeAttached({ timeout: 120000 });
  await firstSpine.scrollIntoViewIfNeeded(); await firstSpine.click();
  await expect(page.getByRole('dialog')).toBeVisible(); await page.keyboard.press('Escape');
  await spine.locator(':scope > summary').click();
  const chartQuerySeconds = (Date.now() - chartStarted) / 1000;
  const pageDigest = async () => createHash('sha256').update(await page.locator('.table-scroll > table').innerText()).digest('hex');
  const originalPage = await pageDigest();
  const exportStarted = Date.now(), download = page.waitForEvent('download', { timeout: 15 * 60 * 1000 });
  await page.getByRole('button', { name: 'Scarica risultati JSON.gz', exact: true }).click();
  const archive = resolve(output, 'results.json.gz'); await (await download).saveAs(archive);
  const exported = await archiveDigest(archive), exportSeconds = (Date.now() - exportStarted) / 1000;
  if (process.env.RESULT_ARCHIVE) expect(exported).toEqual(await archiveDigest(process.env.RESULT_ARCHIVE));
  console.log('RESULT_EXPORT', JSON.stringify({ ...exported, exportSeconds }));
  const importStarted = Date.now();
  const previousResultId = await page.getByLabel('Risultati locali').inputValue();
  await page.getByTestId('reform-file-input').setInputFiles(archive);
  await expect(page.getByLabel('Risultati locali')).not.toHaveValue(previousResultId, { timeout: 15 * 60 * 1000 });
  await expect(page.getByTestId('reform-phase')).toHaveText('imported', { timeout: 15 * 60 * 1000 });
  await expect(messageTable).toHaveText(messageSummaryText, { useInnerText: true, timeout: 120000 });
  expect(await page.getByLabel('Tabella riforma').locator('option').allTextContents()).toEqual(tableOptions);
  await page.getByLabel('Tabella riforma').selectOption({ label: recovery });
  await page.getByRole('button', { name: 'Pagina successiva', exact: true }).click();
  await expect.poll(pageDigest).toBe(originalPage);
  const importSeconds = (Date.now() - importStarted) / 1000;
  const roundtripDownload = page.waitForEvent('download', { timeout: 15 * 60 * 1000 });
  await page.getByRole('button', { name: 'Scarica risultati JSON.gz', exact: true }).click();
  const roundtrip = resolve(output, 'roundtrip.json.gz'); await (await roundtripDownload).saveAs(roundtrip);
  expect(await archiveDigest(roundtrip)).toEqual(exported);
  expect(errors).toEqual([]);
  await cdp.send('HeapProfiler.collectGarbage'); await workerCall('HeapProfiler.collectGarbage');
  samples.push({ phase: 'roundtrip-complete', main: await cdp.send('Runtime.getHeapUsage'), worker: await workerCall('Runtime.getHeapUsage') });
  const heaps = samples as { main: { usedSize: number }; worker: { usedSize?: number } | null }[];
  expect(heaps.every(s => typeof s.worker?.usedSize === 'number'), 'Worker heap measurements must be available').toBe(true);
  expect(Math.max(...heaps.map(s => s.main.usedSize)), 'Retained UI heap').toBeLessThan(32 * 1024 * 1024);
  expect(Math.max(...heaps.map(s => s.worker!.usedSize!)), 'Retained worker heap').toBeLessThan(256 * 1024 * 1024);
  const scenario = JSON.parse(readFileSync(resolve('src/lib/politics/reform-2026/data', catalog.defaultsPath), 'utf8'));
  const report = { created: new Date().toISOString(), browser: context.browser()?.version(), node: process.version, platform: process.platform, arch: process.arch,
    algorithm: scenario.algorithmId, seed: scenario.seed, snapshot: catalog.snapshotVersion, model: catalog.modelVersion,
    verificationResumedFromArchive: !!process.env.RESULT_ARCHIVE, notes: previous?.notes ?? [], computationSeconds, exportSeconds, importSeconds, chartQuerySeconds, scatterKnownPoints,
    compressedArchiveBytes: statSync(archive).size, uncompressedBytes: exported.bytes, archiveSha256: exported.sha256, samples, errors, tableOptions, affectedCount, messageSummaryText };
  writeFileSync(resolve(output, 'report.json'), JSON.stringify(report, null, 2));
  await testInfo.attach('result-memory-report', { body: JSON.stringify(report, null, 2), contentType: 'application/json' });
  } finally {
    await context.close();
    const target = resolve(profile), parent = resolve(tmpdir()) + sep;
    if (!target.startsWith(parent) || !basename(target).startsWith('elezioni-result-acceptance-')) throw new Error('Unsafe test profile cleanup path');
    await rm(target, { recursive: true, force: true, maxRetries: 5, retryDelay: 200 });
  }
});
