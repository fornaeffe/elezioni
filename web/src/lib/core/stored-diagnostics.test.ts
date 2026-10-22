import { beforeEach, expect, it, vi } from 'vitest';
import type { StoredResult, Cell } from './result-store';
import type { ScrutinyWarning } from './types';
import { diagnosticCells, diagnosticColumns, diagnosticTableName } from './diagnostic-messages';
import { storedMessageContext, storedMessageIdPage, storedMessageSummary } from './stored-diagnostics';

const data = vi.hoisted(() => new Map<number, Cell[][]>());
vi.mock('./result-store', async importOriginal => ({ ...await importOriginal<typeof import('./result-store')>(),
  storedRows: async function* (_id: string, table: number) { for (const cells of data.get(table) ?? []) yield cells; }
}));
const warning = (simulationId?: number, extra: Partial<ScrutinyWarning> = {}): ScrutinyWarning => ({ code: 'A', electionKind: 'politiche', severity: 'error', message: 'Original', simulationId, ...extra });
function record(versioned = true, indexed = false): StoredResult {
  return { id: 'test', owner: 'owner', scenario: {} as StoredResult['scenario'], exports: [], result: {
    type: 'result', status: 'not_implemented', warnings: [], tables: [], benchmark: { startedAt: '', dataVersion: '', simulations: 1500, elapsedMs: 1 },
    storage: { id: 'test', state: 'interrupted', completedSimulations: 1201, compressedBytes: 0, maxChunkBytes: 0, warningCounts: { error: 0, warning: 0, info: 0 },
      tables: [{ name: versioned ? diagnosticTableName : 'Warnings and errors', columns: versioned ? diagnosticColumns : ['Code', 'Severity', 'First simulation', 'Message', 'Occurrences'], rows: 0, chunks: 0, runIndexed: indexed }] }
  } };
}
beforeEach(() => data.clear());

it('pages unique ascending IDs beyond 1000 and filters exact context before counting', async () => {
  const events: ScrutinyWarning[] = [];
  for (let id = 1; id <= 1201; id++) events.push(warning(id, { chamber: 'camera', occurrences: 3 }), warning(id, { chamber: 'senato', occurrences: 2, reason: 'propagated' }));
  data.set(0, events.map(diagnosticCells));
  const saved = record();
  expect(await storedMessageIdPage(saved, {}, 1200)).toEqual({ ids: [1201], total: 1201 });
  expect((await storedMessageIdPage(saved)).ids).toHaveLength(50);
  expect(await storedMessageIdPage(saved, { simulation: 1201, reason: 'propagated' })).toEqual({ ids: [1201], total: 1 });
  expect((await storedMessageSummary(saved)).groups[0]).toMatchObject({ simulations: 1201, occurrences: 6005 });
  expect((await storedMessageSummary(saved, { chamber: 'camera' })).groups[0]).toMatchObject({ simulations: 1201, occurrences: 3603 });
  const context = await storedMessageContext(saved, { simulation: 1201 });
  expect(context.rows.map(w => w.chamber)).toEqual(['camera', 'senato']);
});

it('prioritizes global failures/warnings and retains notes beyond the manifest preview', async () => {
  const saved = record();
  data.set(0, Array.from({ length: 71 }, (_, i) => diagnosticCells(warning(undefined, { code: `INFO_${i}`, scope: 'global', severity: 'info' }))));
  const { warningCells } = await import('./result-store');
  const execution = warning(undefined, { code: 'RESULT_STORAGE_ERROR', scope: 'execution' });
  data.set(-1, [warningCells(execution)]); saved.result.warnings = [execution];
  const first = await storedMessageContext(saved, {}, 0, true), second = await storedMessageContext(saved, {}, 50, true);
  expect(first.total).toBe(72); expect(first.rows).toHaveLength(50);
  expect(first.rows[0].code).toBe('RESULT_STORAGE_ERROR'); expect(second.rows).toHaveLength(22);
  expect(second.rows.at(-1)?.code).toBe('INFO_70');
  expect((await storedMessageSummary(saved)).groupCount).toBe(0);
});

it('uses legacy native membership only when storage proves it and refuses imported first-ID aggregates', async () => {
  data.set(0, [['A', 'error', 1, 'Saved aggregate', 50]]);
  expect(await storedMessageSummary(record(false))).toMatchObject({ exact: false, groups: [expect.objectContaining({ occurrences: 50, simulations: 0 })] });
  await expect(storedMessageIdPage(record(false))).rejects.toThrow('non sono ricostruibili');
  expect(await storedMessageSummary(record(false, true))).toMatchObject({ exact: true, groups: [expect.objectContaining({ occurrences: 50, simulations: 1 })] });
  expect(await storedMessageIdPage(record(false, true))).toEqual({ ids: [1], total: 1 });
  const empty = record(false); empty.result.completedSimulations = 0; empty.result.storage!.completedSimulations = 0; empty.result.storage!.tables = [];
  data.clear();
  expect(await storedMessageSummary(empty)).toMatchObject({ exact: true, contextAvailable: true, groupCount: 0 });
  expect(await storedMessageIdPage(empty)).toEqual({ ids: [], total: 0 });
});

it('rejects corrupt ordering even when the filter matches no rows', async () => {
  data.set(0, [diagnosticCells(warning(2)), diagnosticCells(warning(1))]);
  await expect(storedMessageIdPage(record(), { code: 'absent' })).rejects.toThrow('order');
  await expect(storedMessageSummary(record(), { code: 'absent' })).rejects.toThrow('order');
});
