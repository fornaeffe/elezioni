import type { ResultTable } from '../core/types';
import type { SeriesKind } from '../politics/reform-2026/result-series';

export type BonusFilter = 'all' | 'awarded' | 'not-awarded';

export interface SeriesDescriptor {
  id: string; chamber: 'camera' | 'senato'; kind: SeriesKind; owner: string;
  college: string | null; territory: string | null; name: string; color: string; order: number;
}
export interface StatisticalSample {
  resultId: string; series: string; simulation: number;
  x: number | null; y: number | null; entitled: number | null; confirmed: number;
  ordinary: number; bonus: number; special: number; taa: number;
}
export interface SeriesSummary extends SeriesDescriptor {
  observations: number; votesKnown: number; electedKnown: number; voteSum: number; electedSum: number;
  confirmed: number; ordinary: number; bonus: number; special: number; taa: number;
}
export interface StatisticsOverview {
  requested: number; saved: number; legacy: boolean; series: SeriesSummary[]; colleges: SeriesDescriptor[];
}
export interface Distribution {
  count: number; min: number | null; max: number | null; mean: number | null;
  q1: number | null; median: number | null; q3: number | null; p05: number | null; p95: number | null;
  whiskerLow: number | null; whiskerHigh: number | null;
}
export interface SpineBinData { low: number; high: number; count: number; outcomes: { seats: number | null; count: number }[] }
export interface SeriesStatistics {
  descriptor: SeriesDescriptor; requested: number; known: number; missingX: number; missingY: number;
  xDescriptor?: SeriesDescriptor;
  distribution: Distribution; bins: SpineBinData[]; yMax: number;
}

/** Merge ordered series by simulation identity; missing outcomes remain unknown. */
export async function* pairStatisticalSamples(outcomes: AsyncIterable<StatisticalSample>, shares: AsyncIterable<StatisticalSample>): AsyncGenerator<StatisticalSample> {
  const iterator = outcomes[Symbol.asyncIterator]();
  try {
    let outcome = await iterator.next();
    for await (const share of shares) {
      while (!outcome.done && outcome.value.simulation < share.simulation) outcome = await iterator.next();
      const match = !outcome.done && outcome.value.simulation === share.simulation ? outcome.value : undefined;
      yield { ...share, y: match?.y ?? null, entitled: match?.entitled ?? null };
    }
  } finally { await iterator.return?.(); }
}
export const summaryTableName = 'Reform list summary';
export const summaryColumns = ['Chamber', 'List', 'Name', 'Requested', 'Vote observations', 'Election observations', 'Mean vote share %', 'Mean elected', 'Confirmed elected minimum / requested', 'Ordinary minimum / requested', 'Bonus minimum / requested', 'Special minimum / requested', 'TAA minimum / requested'];
export const finiteNumber = (v: unknown): number | null => typeof v === 'number' && Number.isFinite(v) ? v : null;

/** R type-7 quantiles. The caller reads these two ranks from a disk value index. */
export function quantileRanks(count: number, probability: number) {
  const at = Math.max(0, count - 1) * probability;
  return { lower: Math.floor(at), upper: Math.ceil(at), fraction: at - Math.floor(at) };
}
export function interpolated(lower: number, upper: number, fraction: number) { return lower + (upper - lower) * fraction; }
export function emptySummary(d: SeriesDescriptor): SeriesSummary {
  return { ...d, observations: 0, votesKnown: 0, electedKnown: 0, voteSum: 0, electedSum: 0, confirmed: 0, ordinary: 0, bonus: 0, special: 0, taa: 0 };
}
export function addSample(s: SeriesSummary, p: StatisticalSample) {
  s.observations++;
  if (p.x !== null) { s.votesKnown++; s.voteSum += p.x; }
  if (p.y !== null) { s.electedKnown++; s.electedSum += p.y; }
  s.confirmed += p.confirmed; s.ordinary += p.ordinary; s.bonus += p.bonus; s.special += p.special; s.taa += p.taa;
}
export function summaryTable(overview: StatisticsOverview): ResultTable {
  const n = overview.requested;
  return { name: summaryTableName, columns: summaryColumns, rows: overview.series.filter(s => s.kind === 'list').map(s => ({
    Chamber: s.chamber, List: s.owner, Name: s.name, Requested: n, 'Vote observations': s.votesKnown, 'Election observations': s.electedKnown,
    'Mean vote share %': s.votesKnown === n ? s.voteSum / n : null, 'Mean elected': s.electedKnown === n ? s.electedSum / n : null,
    'Confirmed elected minimum / requested': s.confirmed / n, 'Ordinary minimum / requested': s.ordinary / n,
    'Bonus minimum / requested': s.bonus / n, 'Special minimum / requested': s.special / n, 'TAA minimum / requested': s.taa / n
  })) };
}
export function binFor(value: number, boundaries: readonly number[]): number {
  for (let i = 0; i < boundaries.length - 1; i++) if (value < boundaries[i + 1] || i === boundaries.length - 2) return i;
  return 0;
}
