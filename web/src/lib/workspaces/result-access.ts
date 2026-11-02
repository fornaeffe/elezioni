import type { ResultTable, SimulationResult } from '../core/types';

export interface ResultPage { table: number; offset: number; limit?: number; simulation?: number }
/** Memory and local storage expose the same table page, without materializing
 * a stored result in the UI. The owning adapter supplies its storage transport. */
export async function readResultPage(result: SimulationResult, page: ResultPage,
  stored?: (page: ResultPage & { resultId: string }) => Promise<ResultTable>): Promise<ResultTable> {
  const limit = page.limit ?? 50;
  if (![page.table, page.offset, limit].every(n => Number.isSafeInteger(n) && n >= 0) || limit === 0) throw new RangeError('Invalid result page');
  if (result.storage) {
    if (!stored || !result.storage.tables[page.table]) throw new Error('Stored result table unavailable');
    return stored({ ...page, limit, resultId: result.storage.id });
  }
  const table = result.tables[page.table];
  if (!table) throw new Error('Result table unavailable');
  const rows = page.simulation === undefined ? table.rows : table.rows.filter(r => r.Simulation === page.simulation);
  return { ...table, rows: rows.slice(page.offset, page.offset + limit) };
}
