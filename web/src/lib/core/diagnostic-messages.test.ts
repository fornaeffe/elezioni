import { expect, it } from 'vitest';
import { createMessageSummary, diagnosticScope, diagnosticEvent, diagnosticCells, diagnosticColumns } from './diagnostic-messages';
import { streamDiagnosticIdsCsv } from './result-export';
import type { ScrutinyWarning } from './types';

const event = (code: string, simulationId?: number, extra: Partial<ScrutinyWarning> = {}): ScrutinyWarning => ({ code, simulationId, message: 'Original reason', severity: 'error', electionKind: 'politiche', ...extra });

it('counts severity/code unions across reasons and chambers without adding their frequencies', () => {
  const summary = createMessageSummary(10, 3, true);
  summary.add(event('global', undefined, { scope: 'global', occurrences: 20 }));
  summary.add(event('A', 1, { chamber: 'camera', occurrences: 3 }));
  summary.add(event('A', 1, { chamber: 'senato', message: 'Propagated reason', occurrences: 2 }));
  summary.add(event('B', 1));
  summary.add(event('A', 1, { severity: 'warning' }));
  summary.add(event('A', 3));
  const result = summary.finish();
  expect(result).toMatchObject({ requested: 10, processed: 3, exact: true, groupCount: 3 });
  expect(result.totals.error).toMatchObject({ simulations: 2, occurrences: 7 });
  expect(result.groups.map(g => [g.severity, g.code, g.simulations, g.occurrences])).toEqual([['error', 'A', 2, 6], ['error', 'B', 1, 1], ['warning', 'A', 1, 1]]);
});

it('filters original reasons and structured chambers, keeping 100 percent simulation messages out of globals', () => {
  const summary = createMessageSummary(2, 2, true, { chamber: 'senato', reason: 'original', text: 'A' });
  for (const id of [1, 2]) for (const chamber of ['camera', 'senato'] as const) summary.add(event('A', id, { chamber }));
  expect(summary.finish().groups[0]).toMatchObject({ simulations: 2, occurrences: 2 });
  expect(diagnosticScope(event('A', 1))).toBe('simulation');
  expect(diagnosticScope(event('unknown'))).toBe('unknown');
  expect(diagnosticScope(event('RESULT_STORAGE_ERROR'))).toBe('execution');
  expect(diagnosticScope(event('R26_CONDITIONAL_CANDIDATES'))).toBe('global');
});

it('bounds groups and keeps legacy occurrences without claiming exact membership', () => {
  const summary = createMessageSummary(1000, 1000, false);
  for (let i = 0; i < 101; i++) summary.add(event(`C${i}`, 1, { occurrences: 10 }));
  expect(summary.finish().groups).toHaveLength(50);
  expect(summary.finish(100).groups).toHaveLength(1);
  expect(summary.finish().totals.error).toMatchObject({ occurrences: 1010, simulations: 0 });
  expect(summary.finish().exact).toBe(false);
});

it('validates event context and saved coverage and preserves original wording through cells', async () => {
  const original = event('A', 1, { scope: 'simulation', chamber: 'camera', reason: 'origin', occurrences: 4, seatId: 'seat', drawContextId: 'draw', interpretationId: 'I15-v2', lawReference: 'N18' });
  expect(diagnosticEvent(diagnosticCells(original), diagnosticColumns)).toEqual(original);
  expect(() => diagnosticEvent(diagnosticCells({ ...original, occurrences: 0 }), diagnosticColumns)).toThrow();
  const summary = createMessageSummary(3, 2, true);
  expect(() => summary.add(event('A', 3))).toThrow('coverage');
  const unordered = createMessageSummary(3, 3, true); unordered.add(event('A', 2));
  expect(() => unordered.add(event('B', 1))).toThrow('order');
  async function* ids() { yield 1; yield 1201; }
  let csv = ''; for await (const part of streamDiagnosticIdsCsv(ids())) csv += part;
  expect(csv).toBe('Simulation\r\n1\r\n1201\r\n');
});
