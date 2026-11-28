import { seedPresentationSession } from '../fixtures/presentation-session';
import { openResultTables } from '../fixtures/common-page';
import { currentBrowserSession, currentArchive } from '../fixtures/current-browser-session';
import { expect, test } from '../fixtures/editor-preparation';
import { diagnosticResult } from '../fixtures/diagnostic-result';
import { diagnosticCells, diagnosticTableName } from '../../src/lib/core/diagnostic-messages';
import { gunzipSync } from 'fflate';

test('global context precedes exact frequencies and bounded ID inspection, filtering and full CSV', async ({ page }) => {
  test.setTimeout(90000);
  const errors: string[] = []; page.on('pageerror', e => errors.push(e.message));
  const scenario = (await currentBrowserSession()).scenario as unknown as import('../../src/lib/politics/reform-2026/generation-types').ReformGenerationScenario;
  const archive = diagnosticResult(scenario);
  const reordered = { ...archive, result: { ...archive.result, tables: archive.result.tables.map(t => t.name === diagnosticTableName ? { rows: t.rows, columns: t.columns, name: t.name } : t) } };
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await page.getByLabel('Legge elettorale').selectOption(scenario.algorithmId);
  await seedPresentationSession(page, currentArchive(reordered, await currentBrowserSession()));
  const general = page.getByLabel('Informazioni e avvisi generali', { exact: true });
  const summary = page.getByLabel('Messaggi delle simulazioni', { exact: true });
  const sectionStyles = await Promise.all(['Informazioni e avvisi generali', 'Messaggi delle simulazioni',
    'Eletti medi e percentuali sui voti validi', 'Seggi della singola simulazione'].map(name =>
    page.getByRole('region', { name, exact: true }).locator(':scope > summary').evaluate(element => {
      const style = getComputedStyle(element);
      return [style.fontSize, style.fontWeight, style.color, style.cursor];
    })));
  for (const style of sectionStyles) expect(style).toEqual(sectionStyles[0]);
  await general.locator(':scope > summary').click();
  await summary.locator(':scope > summary').click();
  await expect(summary.getByRole('table')).toContainText('1201/1500', { timeout: 30000 });
  await expect(general).toContainText('Model note 25'); // beyond the old 20-note preview
  await expect(general).toContainText('Calcolo interrotto: 1201/1500');
  await expect(general).toContainText('Interrupted test: saved 1201 of 1500');
  await expect(general).toContainText('Ambito non documentato');
  await expect(summary.getByRole('table')).not.toContainText('R26_CONDITIONAL_CANDIDATES');
  await expect(summary.locator('.totals')).toContainText('Errore: 1201/1500');
  await expect(summary.getByRole('table').locator('tbody tr')).toHaveCount(4);
  const row = summary.getByRole('row').filter({ hasText: 'TEST_ERROR' }).filter({ hasText: '6005' });
  await expect(row).toContainText('80,07%');
  const affected = page.getByRole('region', { name: 'Simulazioni interessate', exact: true });
  await expect(affected).toHaveCount(0);
  await summary.getByRole('button', { name: 'Simulazioni per TEST_ERROR (error)', exact: true }).click();
  await expect(affected.locator('.ids li')).toHaveCount(50);
  await expect(affected.locator('.ids li').first()).toContainText('Messaggi di 1');
  await affected.getByRole('button', { name: 'ID successivi', exact: true }).click();
  await expect(affected.locator('.ids li').first()).toContainText('Messaggi di 51');
  const csvDownload = page.waitForEvent('download');
  await affected.getByRole('button', { name: 'Scarica tutti gli ID filtrati CSV', exact: true }).click();
  const stream = await (await csvDownload).createReadStream(); let csv = '';
  for await (const chunk of stream!) csv += chunk.toString();
  expect(csv.trim().split('\r\n')).toEqual(['Simulation', ...Array.from({ length: 1201 }, (_, i) => String(i + 1))]);
  await affected.getByLabel('Cerca ID simulazione', { exact: true }).fill('1201');
  await affected.getByRole('button', { name: 'Filtra simulazioni', exact: true }).click();
  await expect(affected.locator('.ids li')).toHaveCount(1);
  await affected.getByRole('button', { name: 'Messaggi di 1201', exact: true }).click();
  await expect(affected).toContainText('seat-camera'); await expect(affected).toContainText('seat-senato');
  await expect(affected).toContainText('Motivo: origin'); await expect(affected).toContainText('Motivo: propagated');
  await affected.getByRole('button', { name: 'Risultato di 1201', exact: true }).click();
  await openResultTables(page);
  await expect(page.getByLabel('Simulazione (vuoto = tutte)', { exact: true })).toHaveValue('1201');
  await expect(page.getByLabel('Simulazione grafico', { exact: true })).toHaveValue('1201');
  // Editing one view leaves the other alone; repeating inspection synchronizes both again.
  await page.getByLabel('Simulazione grafico', { exact: true }).fill('1');
  await page.getByRole('combobox', { name: 'Ramo grafico', exact: true }).selectOption('senato');
  await expect(page.getByLabel('Simulazione (vuoto = tutte)', { exact: true })).toHaveValue('1201');
  await page.getByLabel('Simulazione (vuoto = tutte)', { exact: true }).fill('2');
  await expect(page.getByLabel('Simulazione grafico', { exact: true })).toHaveValue('1');
  const single = page.getByRole('region', { name: 'Seggi della singola simulazione', exact: true });
  await single.locator(':scope > summary').click();
  await affected.getByRole('button', { name: 'Risultato di 1201', exact: true }).click();
  await expect(page.getByLabel('Simulazione (vuoto = tutte)', { exact: true })).toHaveValue('1201');
  await expect(page.getByLabel('Simulazione grafico', { exact: true })).toHaveValue('1201');
  await expect(page.getByRole('combobox', { name: 'Ramo grafico', exact: true })).toHaveValue('senato');
  // The drill-down belongs to this disclosure and keeps its controls across collapse/reopen.
  await expect(summary.getByRole('region', { name: 'Simulazioni interessate', exact: true })).toBeVisible();
  await summary.locator(':scope > summary').click();
  await expect(affected).not.toBeVisible();
  await summary.locator(':scope > summary').click();
  await expect(affected.getByLabel('Cerca ID simulazione', { exact: true })).toHaveValue('1201');
  await affected.getByLabel('Filtra motivo originale', { exact: true }).fill('propagated');
  await affected.getByRole('button', { name: 'Filtra simulazioni', exact: true }).click();
  await affected.getByRole('button', { name: 'Messaggi di 1201', exact: true }).click();
  await expect(affected).toContainText('seat-senato'); await expect(affected).not.toContainText('seat-camera');
  await summary.getByRole('combobox', { name: 'Ramo dei messaggi', exact: true }).selectOption('camera');
  await summary.getByRole('button', { name: 'Filtra messaggi', exact: true }).click();
  await expect(summary.getByRole('table').locator('tbody tr')).toHaveCount(1);
  await expect(summary.getByRole('table')).toContainText('3603');
  await summary.getByRole('button', { name: 'Simulazioni per TEST_ERROR (error)', exact: true }).click();
  await expect(affected.getByLabel('Cerca ID simulazione', { exact: true })).toHaveValue('');
  await expect(affected.getByLabel('Filtra motivo originale', { exact: true })).toHaveValue('');
  await affected.getByRole('button', { name: 'ID successivi', exact: true }).click();
  await affected.getByRole('button', { name: 'Chiudi elenco delle simulazioni', exact: true }).click();
  await expect(affected).toHaveCount(0);
  await summary.getByRole('button', { name: 'Simulazioni per TEST_ERROR (error)', exact: true }).click();
  await expect(affected.locator('.ids li').first()).toContainText('Messaggi di 1');
  await page.setViewportSize({ width: 390, height: 844 });
  await general.scrollIntoViewIfNeeded();
  await page.getByLabel('Avvisi riforma', { exact: true }).screenshot({ path: 'test-results/reform-messages-mobile.png' });
  expect(await general.evaluate(e => e.scrollWidth)).toBeLessThanOrEqual(await general.evaluate(e => e.clientWidth));

  // Legacy aggregates only know their first simulation, not exact membership.
  const legacy = structuredClone(archive);
  const { completedSimulations: _coverage, ...oldResult } = legacy.result;
  const old = { ...legacy, result: oldResult };
  old.result.tables = old.result.tables.filter(t => t.name !== diagnosticTableName);
  old.result.tables.push({ name: 'Warnings and errors', columns: ['Code', 'Severity', 'First simulation', 'Message', 'Occurrences'], rows: [['TEST_ERROR', 'error', 1, 'Saved aggregate', 6005]] });
  const previous = await page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true }).inputValue();
  await seedPresentationSession(page, currentArchive(old, await currentBrowserSession()));
  await expect(page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true })).not.toHaveValue(previous, {timeout:30000});
  await summary.locator(':scope > summary').click();
  await expect(summary).toContainText('Frequenze e ID esatti non ricostruibili', { timeout: 30000 });
  await expect(summary.getByRole('combobox', { name: 'Ramo dei messaggi', exact: true })).toBeDisabled();
  await expect(summary.getByRole('table')).toContainText('6005');
  await expect(summary.getByRole('button', { name: /Simulazioni per/ })).toHaveCount(0);
  await summary.getByRole('button', { name: 'Testi per TEST_ERROR (error)', exact: true }).click();
  await expect(page.getByRole('region', { name: 'Dettagli messaggio', exact: true })).toContainText('Saved aggregate');

  const zero = structuredClone(archive); zero.result.tables = []; zero.result.completedSimulations = 0;
  zero.result.warnings = [{ code: 'RESULT_STORAGE_ERROR', electionKind: 'politiche', severity: 'error', scope: 'execution', message: 'Failure before the first saved run.' }];
  const beforeZero = await page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true }).inputValue();
  await seedPresentationSession(page, currentArchive(zero, await currentBrowserSession()));
  await expect(page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true })).not.toHaveValue(beforeZero, {timeout:30000});
  await general.locator(':scope > summary').click();
  await summary.locator(':scope > summary').click();
  await expect(general).toContainText('Calcolo interrotto: 0/1500', { timeout: 30000 });
  await expect(summary.locator('.totals')).toContainText('Errore: 0/1500');
  await expect(page.getByRole('alert').filter({ hasText: 'Unknown result table' })).toHaveCount(0);
  const zeroDownload = page.waitForEvent('download');
  await page.getByRole('button', { name: 'Scarica archivio risultati JSON.gz', exact: true }).click();
  const zeroStream = await (await zeroDownload).createReadStream(), zeroChunks: Buffer[] = [];
  for await (const chunk of zeroStream!) zeroChunks.push(Buffer.from(chunk));
  expect(JSON.parse(new TextDecoder().decode(gunzipSync(Buffer.concat(zeroChunks)))).run.result.completedSimulations).toBe(0);
  expect(errors).toEqual([]);
});

test('diagnostic queries preserve pagination, recover from failures and ignore a late reply', async ({ page }) => {
  await page.addInitScript(() => {
    const postMessage = Worker.prototype.postMessage;
    let failedGeneralPage = false;
    Worker.prototype.postMessage = function (message, ...args) {
      const send = () => Reflect.apply(postMessage, this, [message, ...args]);
      const fail = (text: string) => this.dispatchEvent(new MessageEvent('message', { data: {
        type: 'result-service', requestId: message.requestId, ok: false,
        error: { code: 'TEST_DIAGNOSTIC_FAILURE', message: text },
      } }));
      if (message?.action === 'messages-general' && message.offset === 50 && !failedGeneralPage) {
        failedGeneralPage = true;
        document.addEventListener('fail-general-page', () => fail('Pagina generale non caricata.'), { once: true });
        return;
      }
      if (message?.action === 'messages-context' && message.filter?.simulation === 1) {
        document.addEventListener('fail-context-page', () => fail('Contesto non caricato.'), { once: true });
        return;
      }
      if (message?.action !== 'messages-summary') return send();

      if (message.filter?.text === 'fail summary') {
        queueMicrotask(() => fail('Richiesta di prova non riuscita.'));
        return;
      }
      if (message.filter?.text !== 'TEST_INFO') return send();

      document.addEventListener('release-summary-request', send, { once: true });
      const markReply = ({ data }: MessageEvent) => {
        if (data.type !== 'result-service' || data.requestId !== message.requestId) return;
        this.removeEventListener('message', markReply);
        document.documentElement.dataset.summaryProbe = 'replied';
      };
      this.addEventListener('message', markReply);
      document.documentElement.dataset.summaryProbe = 'held';
    };
  });

  const fitFile = await currentBrowserSession();
  const scenario = fitFile.scenario as unknown as import('../../src/lib/politics/reform-2026/generation-types').ReformGenerationScenario;
  const archive = diagnosticResult(scenario);
  const diagnostics = archive.result.tables.find(table => table.name === diagnosticTableName)!;
  diagnostics.rows.push(...Array.from({ length: 51 }, (_, index) => diagnosticCells({
    code: `TEST_PAGE_${index}`, electionKind: 'politiche', scope: 'simulation',
    severity: 'info', simulationId: 1201, message: `Summary pagination sample ${index}`,
  })));
  diagnostics.rows.push(...Array.from({ length: 51 }, (_, index) => diagnosticCells({
    code: `TEST_GENERAL_${index}`, electionKind: 'politiche', scope: 'global',
    severity: 'info', message: `General pagination sample ${index}`,
  })));

  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await page.getByLabel('Legge elettorale').selectOption(scenario.algorithmId);
  await seedPresentationSession(page, currentArchive(archive, fitFile));

  const general = page.getByRole('region', { name: 'Informazioni e avvisi generali', exact: true });
  await general.locator(':scope > summary').click();
  const generalPages = general.getByRole('navigation', { name: 'Pagine messaggi generali' });
  const generalLoading = general.getByText('Caricamento messaggi generali…', { exact: true });
  await expect(generalPages).toContainText('1–50 di 79');
  await general.getByRole('button', { name: 'Generali successivi' }).click();
  await expect(generalLoading).toBeVisible();
  await expect(generalPages).toContainText('1–50 di 79');
  await page.evaluate(() => document.dispatchEvent(new Event('fail-general-page')));
  await expect(general.getByText('Pagina generale non caricata.')).toBeVisible();
  await expect(generalLoading).toBeHidden();
  await general.getByRole('button', { name: 'Riprova a caricare i messaggi generali' }).click();
  await expect(generalPages).toContainText('51–79 di 79');
  await general.locator(':scope > summary').click();

  const summary = page.getByRole('region', { name: 'Messaggi delle simulazioni', exact: true });
  await summary.locator(':scope > summary').click();
  const pagination = summary.getByRole('navigation', { name: 'Pagine riepilogo' });
  await expect(pagination).toContainText('1–50 di 55');
  await summary.getByRole('button', { name: 'Riepilogo successivo' }).click();
  await expect(pagination).toContainText('51–55 di 55');
  await expect(summary.getByRole('table').locator('tbody tr')).toHaveCount(5);

  const search = summary.getByRole('searchbox', { name: 'Cerca nei messaggi' });
  const apply = summary.getByRole('button', { name: 'Filtra messaggi' });
  const loading = summary.getByText('Caricamento riepilogo messaggi…', { exact: true });
  const probe = page.locator('html');
  await search.fill('TEST_INFO');
  await expect(pagination).toContainText('51–55 di 55');
  await apply.click();
  await expect(probe).toHaveAttribute('data-summary-probe', 'held');
  await expect(loading).toBeVisible();
  await search.fill('TEST_OTHER');
  await apply.click();
  const table = summary.getByRole('table');
  await expect(table).toContainText('TEST_OTHER');
  await expect(loading).toBeHidden();
  await page.evaluate(() => document.dispatchEvent(new Event('release-summary-request')));
  await expect(probe).toHaveAttribute('data-summary-probe', 'replied');
  await expect(table).toContainText('TEST_OTHER');
  await expect(table).not.toContainText('TEST_INFO');

  await search.fill('fail summary');
  await apply.click();
  const error = summary.getByText('Richiesta di prova non riuscita.', { exact: true });
  await expect(error).toBeVisible();
  await expect(loading).toBeHidden();
  await expect(search).toHaveValue('fail summary');
  await search.fill('');
  await apply.click();
  await expect(pagination).toContainText('1–50 di 55');
  await expect(error).toBeHidden();

  await summary.getByRole('button', { name: 'Simulazioni per TEST_ERROR (error)', exact: true }).click();
  const affected = summary.getByRole('region', { name: 'Simulazioni interessate', exact: true });
  const contextLoading = affected.getByText('Caricamento messaggi originali…', { exact: true });
  await affected.getByRole('button', { name: 'Messaggi di 1', exact: true }).click();
  await expect(contextLoading).toBeVisible();
  await affected.getByRole('button', { name: 'ID successivi', exact: true }).click();
  const nextContext = affected.getByRole('button', { name: 'Messaggi di 51', exact: true });
  await expect(nextContext).toBeVisible();
  await expect(contextLoading).toBeVisible();
  await page.evaluate(() => document.dispatchEvent(new Event('fail-context-page')));
  const contextError = affected.getByText('Contesto non caricato.');
  await expect(contextError).toBeVisible();
  await expect(contextLoading).toBeHidden();
  await nextContext.click();
  await expect(affected.getByText('Original reason 51', { exact: true })).toHaveCount(2);
  await expect(contextError).toBeHidden();
});
