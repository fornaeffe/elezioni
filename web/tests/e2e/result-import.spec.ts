import { expect, test } from '@playwright/test';
import { gzipSync } from 'node:zlib';

function bundledResultJson(): string {
  return JSON.stringify(
    {
      schema_version: 2,
      exportedAt: '2026-06-05T10:00:00.000Z',
      scenario: {
        id: 'imported-scenario',
        name: 'Imported Scenario',
        electionDate: '2027-05-01',
        defaultSource: {
          kind: 'manual',
          electionKind: 'politiche',
          territory: 'Italia',
          dataVersion: 'v1'
        },
        globalShareMode: 'mean',
        abstentionShare: 40,
        abstentionOverride: false,
        coalitions: [{ id: 'coal-a', name: 'Coalizione A', color: '#224466' }],
        lists: [
          {
            id: 'list-a',
            name: 'Lista A',
            coalition: 'Coalizione A',
            color: '#aa2222',
            startingShare: 50,
            shareOverride: true
          }
        ],
        listCorrespondences: [],
        localShareOverrides: [],
        candidateTemplates: []
      },
      result: {
        type: 'result',
        status: 'completed',
        tables: [
          {
            name: 'Election overview',
            columns: ['Ramo', 'Simulazioni'],
            rows: [['camera', 1]]
          }
        ],
        warnings: [],
        benchmark: {
          startedAt: '2026-06-05T09:59:00.000Z',
          elapsedMs: 42,
          simulations: 1,
          dataVersion: 'v1'
        }
      }
    },
    null,
    2
  );
}

test('rejects uncompressed result JSON from the Results panel', async ({ page }) => {
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-r-parity-v1');

  await page.getByTestId('result-file-input').setInputFiles({
    name: 'imported-results.json',
    mimeType: 'application/json',
    buffer: Buffer.from(bundledResultJson())
  });

  await expect(page.getByLabel('Errori simulazione')).toContainText('RESULT_LOAD_ERROR');
  await expect(page.getByLabel('Errori simulazione')).toContainText('.json.gz');
  await expect(page.getByRole('table', { name: 'Election overview' })).toHaveCount(0);
});

for (const inputId of ['result-file-input', 'scenario-file-input']) {
  test(`rejects old politics archives from ${inputId} without changing the draft or results`, async ({ page }) => {
    await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
    await page.getByLabel('Algoritmo di scrutinio').selectOption('politiche-r-parity-v1');
    await page.getByLabel('Nome scenario', { exact: true }).fill('Current draft');
    await page.getByLabel('Simulazioni', { exact: true }).fill('1');
    await page.getByRole('button', { name: 'Esegui', exact: true }).click();
    const overview = page.getByRole('table', { name: 'Election overview', exact: true });
    await expect(overview).toBeVisible({ timeout: 60_000 });
    const before = await overview.textContent();
    await page.getByTestId(inputId).setInputFiles({ name: 'old-results.json.gz', mimeType: 'application/gzip',
      buffer: gzipSync(Buffer.from(bundledResultJson())) });
    const errors = page.getByLabel('Errori simulazione');
    await expect(errors).toContainText('precedente non supportato');
    await expect(page.getByLabel('Nome scenario', { exact: true })).toHaveValue('Current draft');
    await expect(page.getByLabel('Algoritmo di scrutinio')).toHaveValue('politiche-r-parity-v1');
    await expect(overview).toHaveText(before!);
  });
}
