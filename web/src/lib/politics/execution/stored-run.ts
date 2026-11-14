import { appendResultBatch, cellsWarning, getStoredResult, newResultStorage, saveStoredResult, type Cell, type StoredResult } from '../../core/result-store';
import type { SimulationProgress, SimulationResult } from '../../core/types';
import { ReformComputeClient, runOrderedReformPool, type ReformComputeConnection } from '../../workers/reform-pool';
import type { PoliticsComputePacket } from '../../workers/politics-compute';
import type { PreparedPoliticsExecution } from './prepare';
import { activeGenerator } from '../scenario/registry';
import { requiredAssets } from '../scenario/dependencies';
import { parsePoliticsResult } from '../files/results';
import { executionCheckpoint } from './checkpoint';
import { politicsCapacity, politicsRunResources } from './resources';
import { createReformPresentation } from '../reform-2026/result-presentation';
import { storedSummaryTable } from '../reform-2026/stored-statistics';
import { summaryTableName } from '../../results/statistics';
import { implementationSha256 } from './build';

export async function runStoredPolitics(prepared: PreparedPoliticsExecution, packet: PoliticsComputePacket,
  id: string, owner: string, cancelled: () => boolean, emit: (p: SimulationProgress) => void,
  factory = () => new ReformComputeClient(new Worker(new URL('../../workers/reform-compute.worker.ts', import.meta.url), { type: 'module' })),
  hints = navigator as Navigator & { deviceMemory?: number }, retainedCoordinatorBytes = 0): Promise<StoredResult> {
  const s = prepared.scenario, capacity = politicsCapacity(packet, hints, retainedCoordinatorBytes), workersPlan = politicsRunResources(s, capacity);
  const started = performance.now(), startedAt = new Date().toISOString(), model = activeGenerator(s);
  const execution = { requestedWorkers: workersPlan.requested, computeWorkers: workersPlan.actual, maximumWorkers: capacity.maximum,
    estimatedWorkerMemoryMiB: Math.ceil(capacity.perWorkerBytes / 1048576), estimateVersion: `${capacity.estimateVersion}/common-${prepared.pair.adapterId}-v1`,
    generationMs: 0, scrutinyMs: 0, presentationMs: 0, storageMs: 0 };
  const benchmark = { startedAt, elapsedMs: 0, simulations: s.execution.simulations,
    dataVersion: s.data.historical.version, scrutinyAlgorithmId: s.selection.scrutiny.id, seed: s.execution.simulationSeed, execution };
  const run = parsePoliticsResult({ kind: 'politics-result', schemaVersion: 1, executedScenario: s,
    execution: { generator: model, scrutiny: s.selection.scrutiny, adapter: { id: prepared.pair.adapterId, version: '1.0.0' },
      data: requiredAssets(s), numericalVersion: s.modelSettings[model.id].policies.numerical,
      rngVersion: s.modelSettings[model.id].policies.rng, implementationSha256: implementationSha256(prepared.implementationId),
      requestedWorkers: s.execution.workers, actualWorkers: workersPlan.actual, fittingSeed: s.execution.fittingSeed,
      simulationSeed: s.execution.simulationSeed, startedAt, elapsedMs: 0, ...prepared.keys },
    session: { id, state: 'interrupted', simulations: [] },
    result: { type: 'result', status: 'not_implemented', completedSimulations: 0, tables: [], warnings: [], benchmark } });
  const record: StoredResult = { id, owner, scenario: s, exports: [], politics: { run, checkpoint: await executionCheckpoint(prepared) },
    result: { type: 'result', status: 'not_implemented', completedSimulations: 0, tables: [], warnings: [], benchmark, storage: newResultStorage(id) } };
  // Persist before dispatch: even a zero-prefix interruption has an immutable identity.
  await saveStoredResult(record);
  const presentation = prepared.legalSettings && prepared.context
    ? createReformPresentation(prepared.legalSettings, prepared.context, s.execution.simulations, { includeContext: false }) : undefined;
  const workers: (ReformComputeConnection & { initializePolitics?: (p: PoliticsComputePacket) => Promise<void> })[] = [];
  const progress = (phase: SimulationProgress['phase'], completed: number) => emit({ type: 'progress', phase, completed,
    total: s.execution.simulations, elapsedMs: performance.now() - started });
  let failureCode = 'RESULT_EXECUTION_ERROR';
  const sync = () => {
    record.result.benchmark.elapsedMs = performance.now() - started;
    record.result.tables = record.result.storage!.tables.map(t => ({ name: t.name, columns: t.columns, rows: [] }));
    const run = record.politics!.run;
    record.politics!.run = { ...run, execution: { ...run.execution, elapsedMs: record.result.benchmark.elapsedMs },
      result: { type: 'result', status: record.result.status, completedSimulations: record.result.completedSimulations!,
      tables: record.result.tables.map(t => ({ ...t, rows: [] })), warnings: record.result.warnings.map(w => ({ ...w, electionKind: 'politiche', severity: w.severity ?? 'warning' })),
      benchmark: record.result.benchmark as typeof run.result.benchmark },
      session: { ...run.session, state: record.result.storage!.state === 'complete' ? 'complete' : 'interrupted' } };
  };
  try {
    progress('prepare', 0);
    for (let i = 0; i < workersPlan.actual && !cancelled(); i++) {
      const worker = factory(); workers.push(worker);
      // Transfer owned copies only; the coordinator retains the reusable numeric states.
      await worker.initializePolitics!(structuredClone(packet));
    }
    await runOrderedReformPool(workers, s.execution.simulations, cancelled, async (simulation, worker, ready) => {
      for (const stage of ['generationMs', 'scrutinyMs', 'presentationMs'] as const) execution[stage] += ready.timings[stage];
      for (;;) {
        const chunk = await worker.pull(); if (!chunk) break;
        const started = performance.now(), cells = JSON.parse(new TextDecoder().decode(chunk.buffer)) as Cell[][];
        const tables = ready.tables.map(t => ({ ...t, rows: [] as SimulationResult['tables'][number]['rows'] }));
        const warnings = chunk.table === -1 ? cells.map(cellsWarning) : [];
        if (chunk.table !== -1) {
          const table = tables[chunk.table]; if (!table) throw new Error('Unknown compute table');
          table.rows = cells.map(c => Object.fromEntries(table.columns.map((column, i) => [column, c[i]])));
        }
        failureCode = 'RESULT_STORAGE_ERROR'; await appendResultBatch(record, tables, warnings, simulation);
        failureCode = 'RESULT_EXECUTION_ERROR'; execution.storageMs += performance.now() - started;
      }
      record.result.storage!.completedSimulations = record.result.completedSimulations = simulation;
      const run = record.politics!.run;
      record.politics!.run = { ...run, session: { ...run.session, simulations: [...run.session.simulations,
        { id: simulation, outcome: ready.counts.failed ? 'failed' : ready.counts.partial ? 'partial' : 'complete' }] } };
      sync(); failureCode = 'RESULT_STORAGE_ERROR'; await saveStoredResult(record); failureCode = 'RESULT_EXECUTION_ERROR';
      presentation?.accumulate(ready.counts); progress('simulate', simulation);
    });
    if (cancelled()) {
      record.result.storage!.state = 'interrupted';
      record.result.warnings.push({ code: 'RESULT_CANCELLED', severity: 'warning', electionKind: 'politiche',
        scope: 'execution',
        message: `Calcolo annullato; conservate ${record.result.completedSimulations}/${s.execution.simulations} simulazioni.` });
    } else {
      progress('summarize', s.execution.simulations);
      const summary = await storedSummaryTable(record);
      const final = presentation?.finish() ?? { tables: [summary], warnings: [] };
      const at = final.tables.findIndex(t => t.name === summaryTableName); if (at >= 0) final.tables[at] = summary;
      failureCode = 'RESULT_STORAGE_ERROR';
      // Final tables append after per-run descriptors, rather than corrupt their column contracts.
      const existing = record.result.storage!.tables.map(t => final.tables.find(f => f.name === t.name) ?? { name: t.name, columns: t.columns, rows: [] });
      await appendResultBatch(record, [...existing, ...final.tables.filter(f => !existing.some(t => t.name === f.name))], final.warnings, 0);
      record.result.storage!.state = 'complete'; record.result.status = 'completed';
    }
    sync(); parsePoliticsResult(record.politics!.run); await saveStoredResult(record);
    return record;
  } catch (error) {
    // Discard every uncommitted mutation; never publish a partial simulation.
    const saved = await getStoredResult(id, owner);
    saved.result.storage!.state = 'interrupted'; saved.result.status = 'not_implemented';
    saved.result.warnings.push({ code: failureCode, severity: 'error', electionKind: 'politiche',
      scope: 'execution',
      message: `${String(error)}. Conservate ${saved.result.completedSimulations}/${s.execution.simulations} simulazioni; denominatore originale invariato.` });
    const run = saved.politics!.run;
    saved.politics!.run = { ...run, session: { ...run.session, state: 'interrupted' }, result: { ...run.result, status: 'not_implemented',
      warnings: saved.result.warnings.map(w => ({ ...w, electionKind: 'politiche', severity: w.severity ?? 'warning' })) } };
    await saveStoredResult(saved).catch(() => undefined); return saved;
  } finally { for (const worker of workers) worker.close(); }
}
