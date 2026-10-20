import type { ResultTable, ScrutinyWarning } from '../core/types';
import { maximumResultRecordBytes, resultChunkBytes, resultChunkRows, warningCells } from '../core/result-store';
import { generateReformSimulation, type PreparedReformGeneration } from '../politics/reform-2026/generation';
import { createReformPresentation, type ReformPresentationCounts } from '../politics/reform-2026/result-presentation';
import { runPoliticsSimulation } from '../politics/simulation-scrutiny';
import { REFORM_ALGORITHM_ID } from '../politics/reform-2026/input';

export interface ReformRunReady {
  simulation: number;
  tables: Omit<ResultTable, 'rows'>[];
  counts: ReformPresentationCounts;
  timings: { generationMs: number; scrutinyMs: number; presentationMs: number };
}
export interface ReformRunChunk { table: number; buffer: ArrayBuffer }
export interface ReformRunBatch { tables: ResultTable[]; warnings: ScrutinyWarning[] }

/** The only compute boundary is a complete simulation, including both chambers.
 * Generation/legal errors keep their current per-run failure representation;
 * encoding, messaging and storage failures are infrastructure errors instead.
 */
export function computeReformRun(prepared: PreparedReformGeneration, simulation: number) {
  const presentation = createReformPresentation(prepared.scenario, prepared.snapshot, 1, { includeContext: simulation === 1 });
  const timings = { generationMs: 0, scrutinyMs: 0, presentationMs: 0 };
  let generated: ReturnType<typeof generateReformSimulation> | undefined;
  let output: ReturnType<typeof runPoliticsSimulation> | undefined;
  let failure: unknown, failed = false, started = performance.now();
  try { generated = generateReformSimulation(prepared, simulation); }
  catch (error) { failure = error; failed = true; }
  timings.generationMs = performance.now() - started;
  if (generated) {
    started = performance.now();
    try { output = runPoliticsSimulation({ algorithmId: REFORM_ALGORITHM_ID, simulationId: simulation, input: generated.input }); }
    catch (error) { failure = error; failed = true; }
    timings.scrutinyMs = performance.now() - started;
  }
  started = performance.now();
  if (failed) presentation.failure(simulation, failure);
  else if (generated && output?.algorithmId === REFORM_ALGORITHM_ID) presentation.add(generated.input, output.result, generated.trace);
  else throw new Error('Unexpected reform compute result');
  const batch = presentation.drain();
  timings.presentationMs = performance.now() - started;
  const ready: ReformRunReady = { simulation, tables: batch.tables.map(({ name, columns }) => ({ name, columns })), counts: presentation.counts(), timings };
  return { ready, batch };
}

/** Pull-driven, columnar transport: at most one bounded chunk crosses at a time.
 * The one-record exception and guard match the existing storage contract.
 * Encoding starts only when the coordinator is ready to persist this run.
 */
export function* encodeReformRun(batch: ReformRunBatch): Generator<ReformRunChunk> {
  const encoder = new TextEncoder();
  for (let table = 0; table <= batch.tables.length; table++) {
    const warnings = table === batch.tables.length;
    const source = warnings ? batch.warnings : batch.tables[table].rows;
    let rows: string[] = [], bytes = 2;
    for (const row of source) {
      const cells = warnings ? warningCells(row as ScrutinyWarning)
        : batch.tables[table].columns.map(column => (row as ResultTable['rows'][number])[column] ?? null);
      const text = JSON.stringify(cells), size = encoder.encode(text).byteLength + 1;
      if (size > maximumResultRecordBytes) throw new Error(`Result record exceeds ${maximumResultRecordBytes} bytes`);
      if (rows.length && (bytes + size > resultChunkBytes || rows.length >= resultChunkRows)) {
        yield { table: warnings ? -1 : table, buffer: encoder.encode(`[${rows.join(',')}]`).buffer };
        rows = []; bytes = 2;
      }
      rows.push(text); bytes += size;
    }
    if (rows.length) yield { table: warnings ? -1 : table, buffer: encoder.encode(`[${rows.join(',')}]`).buffer };
    // Release each consumed table before preparing the next one.
    source.length = 0;
  }
}
