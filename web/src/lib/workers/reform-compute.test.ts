import { expect, it } from 'vitest';
import { generationFixture } from '../politics/reform-2026/generation.fixture';
import { generateReformSimulation, prepareReformGeneration } from '../politics/reform-2026/generation';
import { runPoliticsReformScrutiny } from '../politics/reform-2026/scrutiny';
import { createReformPresentation } from '../politics/reform-2026/result-presentation';
import { cellsWarning, resultChunkBytes, resultChunkRows } from '../core/result-store';
import type { ResultTable } from '../core/types';
import { computeReformRun, encodeReformRun } from './reform-compute';

it('separate compute tasks reproduce serial presentation, context, counters, warnings and draws', () => {
  const { scenario, snapshot } = generationFixture(), prepared = prepareReformGeneration(scenario, snapshot);
  const serial = createReformPresentation(scenario, snapshot, 3);
  const summary = createReformPresentation(scenario, snapshot, 3, { includeContext: false });
  const computed = [3, 1, 2].map(simulation => computeReformRun(prepared, simulation));
  for (const simulation of [1, 2, 3]) {
    const generated = generateReformSimulation(prepared, simulation);
    serial.add(generated.input, runPoliticsReformScrutiny(generated.input), generated.trace);
    const expected = serial.drain(), run = computed.find(run => run.ready.simulation === simulation)!;
    const actual = { tables: run.ready.tables.map(table => ({ ...table, rows: [] as ResultTable['rows'] })), warnings: [] as typeof expected.warnings };
    for (const chunk of encodeReformRun(run.batch)) {
      const cells = JSON.parse(new TextDecoder().decode(chunk.buffer)) as (string | number | boolean | null)[][];
      if (chunk.table === -1) actual.warnings.push(...cells.map(cellsWarning));
      else {
        const table = actual.tables[chunk.table];
        table.rows.push(...cells.map(row => Object.fromEntries(table.columns.map((column, index) => [column, row[index]]))));
      }
    }
    expect(actual).toEqual(expected);
    summary.accumulate(run.ready.counts);
    expect(run.batch.tables.every(table => table.rows.length === 0)).toBe(true);
  }
  expect(summary.finish()).toEqual(serial.finish());
});

it('transports columnar rows in bounded chunks without splitting Unicode or changing values', () => {
  const table: ResultTable = { name: 'Details', columns: ['Simulation', 'Text', 'Missing'], rows: Array.from({ length: 700 }, (_, index) => ({ Simulation: 1, Text: `${index} Città 😀 ${'x'.repeat(1500)}`, Missing: null })) };
  const original = structuredClone(table.rows);
  const chunks = [...encodeReformRun({ tables: [table], warnings: [] })];
  expect(chunks.length).toBeGreaterThan(2);
  const restored = chunks.flatMap(chunk => {
    expect(chunk.buffer.byteLength).toBeLessThanOrEqual(resultChunkBytes);
    const cells = JSON.parse(new TextDecoder().decode(chunk.buffer)) as unknown[][];
    expect(cells.length).toBeLessThanOrEqual(resultChunkRows);
    return cells.map(row => Object.fromEntries(table.columns.map((column, index) => [column, row[index]])));
  });
  expect(restored).toEqual(original);
});
