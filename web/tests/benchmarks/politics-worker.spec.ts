import { expect, test } from '@playwright/test';
import { mkdirSync, writeFileSync } from 'node:fs';
import { dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

interface BenchmarkRun {
  simulations_requested: number;
  simulations_completed: number;
  elapsed_ms: number;
  rows_rendered: number;
}

const outputPath = fileURLToPath(
  new URL('../../../test/fixtures/benchmarks/browser_politics_worker.json', import.meta.url)
);

async function runSimulation(page: import('@playwright/test').Page, simulations: number): Promise<BenchmarkRun> {
  const input = page.getByLabel('Simulazioni');
  const runButton = page.getByRole('button', { name: 'Esegui' });

  await input.fill(String(simulations));
  await runButton.click();

  const runsTable = page.getByRole('table', { name: 'Generated pipeline runs' });
  await expect(runsTable.locator('tbody tr')).toHaveCount(simulations * 2, { timeout: 420_000 });
  await expect(page.getByText('POLITICS_DEBUG_PIPELINE_SOURCE')).toBeVisible();

  const rowsRendered = await runsTable.locator('tbody tr').count();
  const elapsedText = (await page.getByTestId('elapsed-ms').textContent()) ?? '0';
  const elapsedMs = Number(elapsedText.replace(/[^\d.]/g, ''));

  expect(rowsRendered).toBe(simulations * 2);
  expect(elapsedMs).toBeGreaterThan(0);

  return {
    simulations_requested: simulations,
    simulations_completed: simulations,
    elapsed_ms: elapsedMs,
    rows_rendered: rowsRendered
  };
}

test('benchmarks the generated politics worker path', async ({ browserName, page }) => {
  test.setTimeout(600_000);

  await page.goto('/');

  const runs: BenchmarkRun[] = [];
  for (const simulations of [10, 100, 1000]) {
    runs.push(await runSimulation(page, simulations));
  }

  const report = {
    metadata: {
      created: new Date().toISOString(),
      browser: browserName,
      workflow: 'politiche',
      path: 'generated TypeScript worker on debug-source snapshot',
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
