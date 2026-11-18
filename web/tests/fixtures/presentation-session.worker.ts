/** Test-only storage seed for retained pre-common result presentation evidence.
 * This is not a runtime file-import route or a converter for common sessions. */
import { handleRetiredResultService as handleResultService } from './retired-result-service';
import { getStoredResult, saveStoredResult } from '../../src/lib/core/result-store';
import { createDefaultCommonPoliticsScenario } from '../../src/lib/politics/scenario/defaults';
import { requiredAssets } from '../../src/lib/politics/scenario/dependencies';
import { requirePair } from '../../src/lib/politics/scenario/registry';
self.onmessage = async ({ data }) => {
  const resultId = crypto.randomUUID(), owner = crypto.randomUUID();
  const reply = await handleResultService({ type: 'result-service', requestId: 1, action: 'import', resultId, owner,
    file: new File([JSON.stringify(data)], 'presentation-evidence.json') });
  if (!reply?.ok) { self.postMessage({ error: reply?.error?.message ?? 'Missing fixture reply' }); return; }
  try {
    // Only synthetic saved-table evidence is adapted here. No production reader
    // accepts these retired files, and this fixture does not assert model parity.
    const record = await getStoredResult(resultId, owner), initial = createDefaultCommonPoliticsScenario();
    const requested = record.result.benchmark.simulations;
    const completed = record.result.completedSimulations ?? record.result.storage!.completedSimulations;
    const scenario = { ...initial, selection: { ...initial.selection, generatorsByLaw: { ...initial.selection.generatorsByLaw,
      [initial.selection.scrutiny.id]: { id: 'M01' as const, version: '2.0.0' } } }, execution: { ...initial.execution, simulations: requested } };
    const benchmark = { ...record.result.benchmark, seed: scenario.execution.simulationSeed,
      scrutinyAlgorithmId: scenario.selection.scrutiny.id, dataVersion: scenario.data.historical.version };
    record.result.benchmark = benchmark;
    // Keep the archived synthetic declarations for reconstruction of sparse
    // tables. The common viewer/session identity is supplied independently.
    record.politics = { checkpoint: null, run: {
      kind: 'politics-result', schemaVersion: 1, executedScenario: scenario,
      execution: { generator: { id: 'M01', version: '2.0.0' }, scrutiny: scenario.selection.scrutiny,
        adapter: { id: requirePair(scenario.selection.scrutiny.id, 'M01').adapterId, version: '1.0.0' },
        data: requiredAssets(scenario), numericalVersion: scenario.modelSettings.M01.policies.numerical,
        rngVersion: scenario.modelSettings.M01.policies.rng, implementationSha256: '0'.repeat(64),
        requestedWorkers: scenario.execution.workers, actualWorkers: 1, fittingSeed: scenario.execution.fittingSeed,
        simulationSeed: scenario.execution.simulationSeed, startedAt: benchmark.startedAt, elapsedMs: benchmark.elapsedMs,
        preparationKey: 'synthetic-presentation', generationKey: 'synthetic-presentation', assemblyKey: 'synthetic-presentation' },
      session: { id: resultId, state: completed === requested ? 'complete' : 'interrupted',
        simulations: Array.from({ length: completed }, (_, i) => ({ id: i + 1, outcome: 'complete' })) },
      result: { type: record.result.type, status: record.result.status, benchmark, completedSimulations: completed, tables: record.result.tables.map(t => ({ ...t, rows: [] })),
        warnings: record.result.warnings.map(w => ({ ...w, electionKind: 'politiche', severity: w.severity ?? 'warning' })) }
    } };
    await saveStoredResult(record);
    self.postMessage({ resultId, owner, scenario, completed, requested });
  } catch (error) { self.postMessage({ error: String(error) }); }
};
