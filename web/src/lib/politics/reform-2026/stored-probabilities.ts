import { storedRows, type StoredResult } from '../../core/result-store';
import type { ResultTable } from '../../core/types';
import { positionAccumulator, positionTable } from './election-probability';
import { ensureStatistics } from './stored-statistics';

async function* rows(record: StoredResult, name: string) {
  const index = record.result.storage!.tables.findIndex(t => t.name === name);
  if (index < 0) return;
  const columns = record.result.storage!.tables[index].columns;
  for await (const cells of storedRows(record.id, index)) yield Object.fromEntries(columns.map((c, i) => [c, cells[i]])) as ResultTable['rows'][number];
}
export async function storedPositionProbabilities(record: StoredResult, chamber: string, list: string) {
  const accumulator = positionAccumulator(record.result.benchmark.simulations);
  for await (const row of rows(record, positionTable)) if (row.Chamber === chamber && row.List === list) accumulator.add(row);
  return { available: record.result.storage!.tables.some(t => t.name === positionTable), requested: record.result.benchmark.simulations, rows: accumulator.rows() };
}
export async function storedBonusProbabilities(record: StoredResult) {
  const { overview } = await ensureStatistics(record);
  const subjects = overview.series.filter(s => s.kind === 'subject').map(s => ({ ...s, wins: 0, known: 0, absent: 0 }));
  // Eligibility retains the actual first subject and the JOINT award decision;
  // do not infer a bonus from seats eventually filled or from vote thresholds.
  let known = 0;
  for await (const row of rows(record, 'Reform runs')) if (row.Bonus === 'not-awarded' || row.Bonus === 'awarded') known++;
  const awarded = new Map<string, { wins: number; last: number }>();
  for await (const row of rows(record, 'Bonus eligibility')) if (row.Status === 'awarded') {
    const key = JSON.stringify([row.Chamber, row.Subject]), count = awarded.get(key) ?? { wins: 0, last: 0 };
    if (count.last !== row.Simulation) count.wins++;
    count.last = Number(row.Simulation); awarded.set(key, count);
  }
  for (const subject of subjects) { subject.wins = awarded.get(JSON.stringify([subject.chamber, subject.owner]))?.wins ?? 0; subject.known = known; }
  return subjects;
}
