import type { ScrutinyWarning } from '../../src/lib/core/types';
import type { ReformGenerationScenario } from '../../src/lib/politics/reform-2026/generation-types';
import { diagnosticTableName, diagnosticColumns, diagnosticCells } from '../../src/lib/core/diagnostic-messages';
import { statisticalResult } from './statistical-result';

/** Synthetic diagnostics: overlapping code/severity, contexts and interruption. */
export function diagnosticResult(scenario: ReformGenerationScenario) {
  const archive = statisticalResult(scenario);
  const events: ScrutinyWarning[] = Array.from({ length: 25 }, (_, i) => ({ code: `GLOBAL_NOTE_${i + 1}`, electionKind: 'politiche', scope: 'global', severity: 'info', message: `Model note ${i + 1}` }));
  events.push({ code: 'R26_CONDITIONAL_CANDIDATES', electionKind: 'politiche', scope: 'global', severity: 'warning', message: 'Synthetic candidate conclusions are conditional.' },
    { code: 'UNKNOWN_CONTEXT', electionKind: 'politiche', severity: 'warning', message: 'Scope was not recorded.' });
  for (let simulationId = 1; simulationId <= 1201; simulationId++) {
    for (const chamber of ['camera', 'senato'] as const) events.push({ code: 'TEST_ERROR', electionKind: 'politiche', scope: 'simulation', severity: 'error', simulationId,
      chamber, occurrences: chamber === 'camera' ? 3 : 2, message: `Original reason ${simulationId}`, reason: chamber === 'camera' ? 'origin' : 'propagated', seatId: `seat-${chamber}`, interpretationId: 'test-interpretation', lawReference: 'test-law', drawContextId: 'test-draw' });
    if (simulationId % 2 === 0) events.push({ code: 'TEST_OTHER', electionKind: 'politiche', scope: 'simulation', severity: 'error', simulationId, message: 'Other error' },
      { code: 'TEST_ERROR', electionKind: 'politiche', scope: 'simulation', severity: 'warning', simulationId, message: 'Distinct severity' });
    if (simulationId === 1) events.push({ code: 'TEST_INFO', electionKind: 'politiche', scope: 'simulation', severity: 'info', simulationId, message: 'Run information' });
  }
  archive.result.tables.push({ name: diagnosticTableName, columns: diagnosticColumns, rows: events.map(diagnosticCells) });
  archive.result.status = 'not_implemented'; archive.result.benchmark.simulations = 1500;
  return { ...archive, result: { ...archive.result, completedSimulations: 1201, warnings: [...events, { code: 'RESULT_CANCELLED', electionKind: 'politiche', scope: 'execution', severity: 'warning', message: 'Interrupted test: saved 1201 of 1500 requested.' } satisfies ScrutinyWarning] } };
}
