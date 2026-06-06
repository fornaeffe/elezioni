import { expect, test } from '@playwright/test';
import { existsSync, mkdirSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

interface BenchmarkRun {
  simulations_requested: number;
  simulations_completed: number;
  elapsed_ms: number;
  rows_rendered: number;
}

const outputPath = fileURLToPath(
  new URL('../../../test/fixtures/benchmarks/browser_regional_er_worker.json', import.meta.url)
);
const hasRegionalErStaticSnapshot = existsSync(resolve(process.cwd(), 'static/data/v1/regional-er-static.json'));

test.skip(
  !hasRegionalErStaticSnapshot,
  'Generated regional Emilia-Romagna static snapshot is missing. Run scripts/export_regional_er_static_snapshot.R first.'
);

async function runSimulation(page: import('@playwright/test').Page, simulations: number): Promise<BenchmarkRun> {
  const input = page.getByLabel('Simulazioni');
  const runButton = page.getByRole('button', { name: 'Esegui' });

  await input.fill(String(simulations));
  await runButton.click();

  await expect(page.getByRole('table', { name: 'Regional average seats by list' })).toBeVisible({
    timeout: 240_000
  });
  await page.getByRole('button', { name: 'Mostra dettagli' }).click();

  const runsTable = page.getByRole('table', { name: 'Generated regional runs' });
  await expect(runsTable.locator('tbody tr')).toHaveCount(simulations, { timeout: 240_000 });
  await expect(page.getByText('REGIONAL_ER_STATIC_SNAPSHOT')).toBeVisible();

  const rowsRendered = await runsTable.locator('tbody tr').count();
  const elapsedText = (await page.getByTestId('elapsed-ms').textContent()) ?? '0';
  const elapsedMs = Number(elapsedText.replace(/[^\d.]/g, ''));

  expect(rowsRendered).toBe(simulations);
  expect(elapsedMs).toBeGreaterThan(0);

  return {
    simulations_requested: simulations,
    simulations_completed: simulations,
    elapsed_ms: elapsedMs,
    rows_rendered: rowsRendered
  };
}

test('benchmarks the generated Emilia-Romagna regional worker path', async ({ browserName, page }) => {
  test.setTimeout(420_000);

  await page.goto('/emilia-romagna');

  const runs: BenchmarkRun[] = [];
  for (const simulations of [10, 100, 1000]) {
    runs.push(await runSimulation(page, simulations));
  }

  const report = {
    metadata: {
      created: new Date().toISOString(),
      browser: browserName,
      workflow: 'regionali-er',
      path: 'generated TypeScript worker on production Emilia-Romagna static snapshot exported from current R preparation pipeline',
      data_version: 'v1',
      notes: [
        'Elapsed time is read from SimulationResult.benchmark.elapsedMs as rendered by the app.',
        'The worker path currently runs in 50-simulation chunks and is capped at 1000 simulations.'
      ]
    },
    runs
  };

  mkdirSync(dirname(outputPath), { recursive: true });
  writeFileSync(outputPath, `${JSON.stringify(report, null, 2)}\n`);

  console.table(
    runs.map((run) => ({
      simulations: run.simulations_completed,
      elapsed_s: (run.elapsed_ms / 1000).toFixed(3)
    }))
  );
});
