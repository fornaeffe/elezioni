import type { ResultTable, ScrutinyWarning } from '../core/types';
import { maximumResultRecordBytes, resultChunkBytes, resultChunkRows, warningCells } from '../core/result-store';
import { generateReformSimulation, type PreparedReformGeneration } from '../politics/reform-2026/generation';
import { createReformPresentation, type ReformPresentationCounts } from '../politics/reform-2026/result-presentation';
import { runPoliticsSimulation } from '../politics/simulation-scrutiny';
import { REFORM_ALGORITHM_ID } from '../politics/reform-2026/input';
import { prepareFastGeneration } from '../politics/reform-2026/vote-share/generation';
import { requireFastScenario } from '../politics/reform-2026/vote-share/scenario';
import { generationKeys } from '../politics/reform-2026/vote-share/release';
import type { GenerationPacket } from '../politics/reform-2026/vote-share/fit-types';
import { sourceKey } from '../politics/reform-2026/vote-share/source-codec';
import { fastReformImplementations } from '../politics/reform-2026/vote-share/build';
export async function prepareFastCompute(packet:GenerationPacket){
  if(fastReformImplementations&&packet.scenario.models.generationImplementationId!==fastReformImplementations.generation) throw new Error('Generation implementation differs from this application build');
  const scenario=requireFastScenario(packet.scenario),keys=await generationKeys(scenario,packet.context,await sourceKey('conditional-electorates-v1',{
    codes:packet.assets.electorates.map(e=>e.code),date:packet.assets.electorates[0].date,camera:Uint32Array.from(packet.assets.electorates,e=>e.camera),senato:Uint32Array.from(packet.assets.electorates,e=>e.senato),provenance:packet.assets.electorates[0].provenance
  }));
  // The runtime electorate key uses typed columns; its original ordering is preserved.
  if(keys.runKey!==packet.keys.runKey||keys.compositionKey!==packet.keys.compositionKey)throw new Error('Frozen run binding mismatch');
  const prepared=prepareFastGeneration(scenario,packet.assets,packet.context);
  return{scenario,snapshot:packet.context,generate:(id:number)=>prepared.run(id)};
}
export type PreparedFastCompute=Awaited<ReturnType<typeof prepareFastCompute>>;

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
export function computeReformRun(prepared: PreparedReformGeneration|PreparedFastCompute, simulation: number) {
  const presentation = createReformPresentation(prepared.scenario, prepared.snapshot, 1, { includeContext: simulation === 1 });
  const timings = { generationMs: 0, scrutinyMs: 0, presentationMs: 0 };
  let generated: ReturnType<typeof generateReformSimulation> | undefined;
  let output: ReturnType<typeof runPoliticsSimulation> | undefined;
  let failure: unknown, failed = false, started = performance.now();
  try { generated = 'generate' in prepared?prepared.generate(simulation):generateReformSimulation(prepared, simulation); }
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
