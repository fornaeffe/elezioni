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
  await page.goto('/politics');

  await page.getByTestId('result-file-input').setInputFiles({
    name: 'imported-results.json',
    mimeType: 'application/json',
    buffer: Buffer.from(bundledResultJson())
  });

  await expect(page.getByLabel('Avvisi simulazione')).toContainText('RESULT_LOAD_ERROR');
  await expect(page.getByLabel('Avvisi simulazione')).toContainText('.json.gz');
  await expect(page.getByRole('table', { name: 'Election overview' })).toHaveCount(0);
});

test('imports compressed columnar result JSON from the Results panel', async ({ page }) => {
  await page.goto('/politics');

  await page.getByTestId('result-file-input').setInputFiles({
    name: 'imported-results.json.gz',
    mimeType: 'application/gzip',
    buffer: gzipSync(Buffer.from(bundledResultJson()))
  });

  await expect(page.getByRole('heading', { name: 'Imported Scenario' })).toBeVisible();
  await expect(page.getByLabel('Note simulazione')).toContainText('RESULT_IMPORT');
  await expect(page.getByRole('table', { name: 'Election overview' })).toBeVisible();
  await expect(page.getByRole('button', { name: 'Scarica risultati compressi' })).toBeVisible();
});

test('scenario upload accepts compressed bundled result JSON and shows the imported results', async ({ page }) => {
  await page.goto('/politics');

  await page.getByTestId('scenario-file-input').setInputFiles({
    name: 'scenario-and-results.json.gz',
    mimeType: 'application/gzip',
    buffer: gzipSync(Buffer.from(bundledResultJson()))
  });

  await expect(page.getByRole('heading', { name: 'Imported Scenario' })).toBeVisible();
  await expect(page.getByLabel('Note simulazione')).toContainText('RESULT_IMPORT');
  await expect(page.getByRole('table', { name: 'Election overview' })).toBeVisible();
});
