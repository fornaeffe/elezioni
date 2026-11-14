/** Common scenarios reuse the bounded one-simulation transport and ordered pool. */
import type { PreparedPoliticsCompute } from '../politics/execution/compute-inputs';
import { runPoliticsSimulation } from '../politics/simulation-scrutiny';
import { createReformPresentation } from '../politics/reform-2026/result-presentation';
import { buildPoliticsResultPlotTables, summarizePoliticsGeneratedRuns, summarizePoliticsScrutinyRun } from '../politics/result-presentation';
import { rosatellumSeries } from '../politics/execution/result-presentation';
import { diagnosticTableName, diagnosticColumns, diagnosticCells } from '../core/diagnostic-messages';
import { REFORM } from '../politics/scenario/schema';
import type { ReformRunReady, ReformRunBatch } from './reform-compute';
import type { ScrutinyWarning } from '../core/types';
export { preparePoliticsCompute, type PoliticsComputePacket } from '../politics/execution/compute-packet';
export function computePoliticsRun(prepared: PreparedPoliticsCompute, simulation: number) {
  const timings = { generationMs: 0, scrutinyMs: 0, presentationMs: 0 };
  const presentation = prepared.legalSettings && prepared.context ? createReformPresentation(prepared.legalSettings,
    prepared.context, 1, { includeContext: simulation === 1, identity: { selection: prepared.scenario.selection, pair: prepared.pair, implementationId: prepared.implementationId, keys: prepared.keys } }) : undefined;
  let generated: ReturnType<PreparedPoliticsCompute['generate']> | undefined;
  let output: ReturnType<typeof runPoliticsSimulation> | undefined, failure: unknown, failed = false;
  let failedChamber: 'camera' | 'senato' | undefined;
  const chamberTimes = new Map<string, number>();
  let started = performance.now();
  try { generated = prepared.generate(simulation); } catch (error) { failure = error; failed = true; }
  timings.generationMs = performance.now() - started;
  if (generated) {
    started = performance.now();
    const completed = new Set<string>();
    try { output = runPoliticsSimulation(generated.request, undefined, (ch, elapsedMs) => { completed.add(ch); chamberTimes.set(ch, elapsedMs); }); }
    catch (error) {
      failure = error; failed = true;
      if (generated.request.algorithmId !== REFORM) failedChamber = generated.request.chambers.find(c => !completed.has(c.context.ramo))?.context.ramo;
    }
    timings.scrutinyMs = performance.now() - started;
  }
  started = performance.now();
  let batch: ReformRunBatch, counts: ReformRunReady['counts'];
  if (presentation) {
    if (failed) presentation.failure(simulation, failure);
    else if (generated?.request.algorithmId === REFORM && output?.algorithmId === REFORM)
      presentation.add(generated.request.input, output.result, generated.votes.trace);
    else throw new Error('Unexpected reform compute result');
    batch = presentation.drain(); counts = presentation.counts();
  } else {
    const warnings: ScrutinyWarning[] = (generated?.votes.trace.warnings ?? []).map(w => ({ ...w, severity: 'warning', electionKind: 'politiche', simulationId: simulation }));
    if (failed) warnings.push({ code: 'ROSATELLUM_EXECUTION_ERROR', severity: 'error', electionKind: 'politiche', simulationId: simulation,
      scope: 'simulation', chamber: failedChamber, reason: generated ? 'scrutiny-failed' : 'generation-failed',
      message: failure instanceof Error ? failure.message : String(failure) });
    const summaries = !failed && output?.algorithmId !== REFORM && generated?.request.algorithmId !== REFORM && output && generated ?
      output.chambers.map(ch => { const input = generated.request.algorithmId !== REFORM ? generated.request.chambers.find(c => c.context.ramo === ch.chamber)! : undefined;
        return summarizePoliticsScrutinyRun({ ramo: ch.chamber, sim: simulation, elapsedMs: chamberTimes.get(ch.chamber) ?? timings.scrutinyMs,
          input: input!.input, output: ch.output, listeNaz: input!.context.liste_naz }); }) : [];
    batch = { tables: [summarizePoliticsGeneratedRuns(summaries),
      ...buildPoliticsResultPlotTables(summaries), rosatellumSeries(prepared.scenario, summaries),
      { name: diagnosticTableName, columns: diagnosticColumns, rows: warnings.map(w => Object.fromEntries(diagnosticColumns.map((c, i) => [c, diagnosticCells(w)[i]]))) }], warnings };
    counts = { complete: failed ? 0 : 1, partial: 0, failed: failed ? 1 : 0, awarded: 0 };
  }
  timings.presentationMs = performance.now() - started;
  const ready: ReformRunReady = { simulation, timings, counts, tables: batch.tables.map(({ name, columns }) => ({ name, columns })) };
  return { ready, batch };
}
