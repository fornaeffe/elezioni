import { fixture } from '../scenario/scenario.fixture';
import { selectGenerator } from '../scenario/edits';
import { activeGenerator, requirePair } from '../scenario/registry';
import { requiredAssets } from '../scenario/dependencies';
import type { PoliticsArchiveEnvelope, PoliticsResultEnvelope } from '../scenario/envelopes';

export function run(s = selectGenerator(fixture().scenario, 'M01', '2.0.0')): PoliticsResultEnvelope {
  const generator = activeGenerator(s), policy = s.modelSettings[generator.id].policies;
  return { kind: 'politics-result', schemaVersion: 1, executedScenario: s,
    execution: { generator, scrutiny: s.selection.scrutiny, adapter: { id: requirePair(s.selection.scrutiny.id, generator.id).adapterId, version: '1.0.0' },
      data: requiredAssets(s), numericalVersion: policy.numerical, rngVersion: policy.rng, implementationSha256: 'b'.repeat(64),
      requestedWorkers: s.execution.workers, actualWorkers: 2, fittingSeed: s.execution.fittingSeed, simulationSeed: s.execution.simulationSeed,
      startedAt: '2026-11-08T10:00:00.000Z', elapsedMs: 123, preparationKey: 'preparation', generationKey: 'generation', assemblyKey: 'assembly' },
    session: { id: 'session-1', state: 'interrupted', simulations: [{ id: 1, outcome: 'complete' }, { id: 2, outcome: 'failed' }] },
    result: { type: 'result', status: 'completed', completedSimulations: 2,
      benchmark: { startedAt: '2026-11-08T10:00:00.000Z', elapsedMs: 123, simulations: s.execution.simulations,
        dataVersion: s.data.historical.version, scrutinyAlgorithmId: s.selection.scrutiny.id, seed: s.execution.simulationSeed },
      tables: [{ name: 'Results', columns: ['id', 'name', 'value', 'missing'], rows: [[1, 'Lista, "A"', 0, null], [2, '', false, null]] }],
      warnings: [{ code: 'FAILED', severity: 'error', electionKind: 'politiche', message: 'Retained failure',
        simulationId: 2, chamber: 'camera', scope: 'simulation', occurrences: 1, reason: 'scrutiny-failed' }] } };
}
export function archive(r = run()): PoliticsArchiveEnvelope {
  return { kind: 'politics-result-archive', schemaVersion: 1, exportedAt: '2026-11-08T10:01:00Z', run: r, portableFit: null };
}
