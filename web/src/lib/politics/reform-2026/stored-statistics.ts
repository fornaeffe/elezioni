import { storedRows, type StoredResult } from '../../core/result-store';
import { deleteStatisticsCache, orderedValue, readStatisticsCache, samples, samplesByValue, saveStatisticsCache, writeStatisticalSamples, type StatisticsCache } from '../../core/statistics-cache';
import { addSample, binFor, emptySummary, finiteNumber, interpolated, quantileRanks, summaryTable, type BonusFilter, type Distribution, type SeriesDescriptor, type SeriesStatistics, type SeriesSummary, type StatisticalSample } from '../../results/statistics';
import type { ResultTable } from '../../core/types';
import { reformCollegeSeriesTable, reformSeriesTable, seriesKey, unaffiliatedOwner } from './result-series';
import { reformPresentation, subjectColor } from './presentation-metadata';

type Row = ResultTable['rows'][number];
const pending = new Map<string, Promise<StatisticsCache>>();
const fingerprint = (r: StoredResult) => JSON.stringify([2, r.result.storage!.completedSimulations, r.result.storage!.tables.filter(t => [reformSeriesTable, reformCollegeSeriesTable, 'Reform vote shares', 'Seat entitlement and election locations'].includes(t.name))]);
async function* tableRows(record: StoredResult, name: string): AsyncGenerator<Row> {
  const index = record.result.storage!.tables.findIndex(t => t.name === name);
  if (index < 0) return;
  const columns = record.result.storage!.tables[index].columns;
  for await (const cells of storedRows(record.id, index)) yield Object.fromEntries(columns.map((c, i) => [c, cells[i]]));
}
export function seriesObservation(record: StoredResult, row: Row): { descriptor: SeriesDescriptor; sample: StatisticalSample } {
  if (!['camera', 'senato'].includes(String(row.Chamber)) || !['list', 'subject', 'college'].includes(String(row.Kind)) ||
    !Number.isSafeInteger(row.Simulation) || Number(row.Simulation) < 1 || Number(row.Simulation) > record.result.benchmark.simulations) throw new Error('Invalid statistical series row');
  const descriptor: SeriesDescriptor = { id: seriesKey(String(row.Chamber), String(row.Kind), String(row.Owner), row.College === null ? null : String(row.College)),
    chamber: row.Chamber as SeriesDescriptor['chamber'], kind: row.Kind as SeriesDescriptor['kind'], owner: String(row.Owner), college: row.College === null ? null : String(row.College),
    territory: row.Territory === null ? null : String(row.Territory), name: String(row.Name), color: /^#[0-9a-f]{6}$/i.test(String(row.Color)) ? String(row.Color) : '#64748b', order: Number(row.Order) || 0 };
  const sample: StatisticalSample = { resultId: record.id, series: descriptor.id, simulation: Number(row.Simulation), x: finiteNumber(row['Vote share %']),
    y: finiteNumber(row.Elected), entitled: finiteNumber(row.Entitled), confirmed: finiteNumber(row.Confirmed) ?? 0,
    ordinary: finiteNumber(row.Ordinary) ?? 0, bonus: finiteNumber(row.Bonus) ?? 0, special: finiteNumber(row.Special) ?? 0, taa: finiteNumber(row.TAA) ?? 0 };
  if (sample.x !== null && (sample.x < 0 || sample.x > 100) || [sample.y, sample.entitled, sample.confirmed, sample.ordinary, sample.bonus, sample.special, sample.taa].some(n => n !== null && (!Number.isSafeInteger(n) || n < 0))) throw new Error('Invalid statistical observation');
  return { descriptor, sample };
}

/** Canonical summaries need only O(subjects) counters and no derived writes.
 * Cache quota exhaustion must never interrupt an otherwise complete election. */
export async function storedSummaryTable(record: StoredResult) {
  const summaries = new Map<string, SeriesSummary>();
  for await (const row of tableRows(record, reformSeriesTable)) {
    const { descriptor, sample } = seriesObservation(record, row);
    const summary = summaries.get(descriptor.id) ?? emptySummary(descriptor);
    addSample(summary, sample); summaries.set(descriptor.id, summary);
  }
  return summaryTable({ requested: record.result.benchmark.simulations, saved: record.result.storage!.completedSimulations,
    legacy: false, series: [...summaries.values()], colleges: [] });
}

/** Compatibility reads only archived evidence. Ordinary/recovered list locations
 * can prove an affiliation; original coalition bonus/special ownership cannot. */
async function* legacyRows(record: StoredResult): AsyncGenerator<Row> {
  if (!('kind' in record.scenario)) return;
  const scenario = record.scenario, presentation = reformPresentation(scenario);
  async function* groups(name: string) {
    let simulation = -1, rows: Row[] = [];
    for await (const row of tableRows(record, name)) {
      const id = Number(row.Simulation);
      if (simulation !== id && rows.length) { yield { simulation, rows }; rows = []; }
      if (id < simulation) throw new Error('Archived series are not in simulation order');
      simulation = id; rows.push(row);
    }
    if (rows.length) yield { simulation, rows };
  }
  const ledgers = groups('Seat entitlement and election locations'); let ledger = await ledgers.next();
  for await (const votes of groups('Reform vote shares')) {
    while (!ledger.done && ledger.value.simulation < votes.simulation) ledger = await ledgers.next();
    const entries = !ledger.done && ledger.value.simulation === votes.simulation ? ledger.value.rows : [];
    for (const chamber of ['camera', 'senato'] as const) {
      const voteRows = votes.rows.filter(r => r.Chamber === chamber); if (!voteRows.length) continue;
      const seats = entries.filter(r => r.Chamber === chamber), filled = seats.filter(r => r.Status === 'filled');
      const final = seats.length === (chamber === 'camera' ? 392 : 196) && seats.every(r => r.Status !== 'unresolved');
      const affiliation = (r: Row) => r.Category === 'ordinary' || r.Category === 'taa-proportional' || r.Category === 'bonus' && r['Election college'] !== null ? r['Election list'] : null;
      const exactLists = final && filled.every(r => affiliation(r) !== null);
      const lists = scenario.lists[chamber];
      function row(owner: string, kind: string, name: string, color: string, order: number, selected: Row[], v: number | null, known: boolean): Row {
        const valid = finiteNumber(voteRows[0]['Valid votes']);
        return { Simulation: votes.simulation, Chamber: chamber, Kind: kind, Owner: owner, College: null, Territory: null, Name: name, Color: color, Order: order,
          Votes: v, 'Valid votes': valid, 'Vote share %': v !== null && valid !== null && valid > 0 ? v / valid * 100 : null,
          Elected: known ? selected.length : null, Confirmed: selected.length, Ordinary: selected.filter(s => s.Category === 'ordinary').length,
          Bonus: selected.filter(s => s.Category === 'bonus').length, Special: selected.filter(s => s.Category === 'special-uninominal').length,
          TAA: selected.filter(s => s.Category === 'taa-proportional').length, Entitled: null };
      }
      for (const [i, l] of lists.entries()) yield row(`list:${l.id}`, 'list', l.name, presentation.lists.find(p => p.id === l.id)?.color ?? subjectColor(l.id), i,
        filled.filter(r => affiliation(r) === l.id), finiteNumber(voteRows.find(r => r.List === l.id)?.Votes), exactLists);
      yield row(unaffiliatedOwner, 'list', 'Affiliazione non ricostruibile dall’archivio', '#64748b', lists.length, filled.filter(r => affiliation(r) === null), null, false);
      const owners = [...new Set(lists.map(l => l.coalitionId ? `coalition:${l.coalitionId}` : `list:${l.id}`))];
      for (const [i, owner] of owners.entries()) {
        const members = lists.filter(l => owner === (l.coalitionId ? `coalition:${l.coalitionId}` : `list:${l.id}`));
        const values = members.map(l => finiteNumber(voteRows.find(r => r.List === l.id)?.Votes));
        const id = owner.slice(owner.indexOf(':') + 1), p = presentation.coalitions.find(p => p.id === id);
        yield row(owner, 'subject', p?.name ?? members.map(l => l.name).join(' + '), p?.color ?? subjectColor(id), i,
          filled.filter(r => r.Subject === owner), values.every(v => v !== null) ? values.reduce<number>((n, v) => n + v!, 0) : null, final);
      }
    }
  }
}

export async function ensureStatistics(record: StoredResult): Promise<StatisticsCache> {
  const key = record.id, version = fingerprint(record), cached = await readStatisticsCache(key);
  if (cached?.fingerprint === version) return cached;
  const existing = pending.get(key); if (existing) return existing;
  const task = (async () => {
    await deleteStatisticsCache(key);
    const legacy = !record.result.storage!.tables.some(t => t.name === reformSeriesTable);
    const summaries = new Map<string, SeriesSummary>(), last = new Map<string, number>(); let batch: StatisticalSample[] = [];
    for await (const row of legacy ? legacyRows(record) : tableRows(record, reformSeriesTable)) {
      const { descriptor, sample } = seriesObservation(record, row);
      if ((last.get(descriptor.id) ?? 0) >= sample.simulation) throw new Error('Duplicate or unordered statistical observation');
      last.set(descriptor.id, sample.simulation);
      const summary = summaries.get(descriptor.id) ?? emptySummary(descriptor); addSample(summary, sample); summaries.set(descriptor.id, summary);
      batch.push(sample); if (batch.length === 256) { await writeStatisticalSamples(batch); batch = []; }
    }
    if (batch.length) await writeStatisticalSamples(batch);
    const colleges: SeriesDescriptor[] = []; let firstSimulation: number | undefined;
    if (!legacy) for await (const row of tableRows(record, reformCollegeSeriesTable)) {
      if (firstSimulation !== undefined && row.Simulation !== firstSimulation) break;
      firstSimulation = Number(row.Simulation); colleges.push(seriesObservation(record, row).descriptor);
    }
    const cache: StatisticsCache = { id: key, fingerprint: version, locals: [], overview: { requested: record.result.benchmark.simulations,
      saved: record.result.storage!.completedSimulations, legacy, series: [...summaries.values()].sort((a, b) => a.chamber.localeCompare(b.chamber) || a.order - b.order), colleges } };
    await saveStatisticsCache(cache); return cache;
  })();
  pending.set(key, task);
  try { return await task; } catch (e) { await deleteStatisticsCache(key).catch(() => undefined); throw e; }
  finally { pending.delete(key); }
}

async function ensureLocal(record: StoredResult, cache: StatisticsCache, id: string) {
  const existing = cache.locals.find(l => l.descriptor.id === id); if (existing) return existing.summary;
  const descriptor = cache.overview.colleges.find(c => c.id === id); if (!descriptor) throw new Error('Statistical series unavailable in this archive');
  const summary = emptySummary(descriptor); let batch: StatisticalSample[] = [], last = 0;
  for await (const row of tableRows(record, reformCollegeSeriesTable)) {
    if (row.Chamber !== descriptor.chamber || row.Owner !== descriptor.owner || row.College !== descriptor.college) continue;
    const { sample } = seriesObservation(record, row);
    if (sample.simulation <= last) throw new Error('Duplicate or unordered college observation');
    last = sample.simulation; addSample(summary, sample); batch.push(sample);
    if (batch.length === 256) { await writeStatisticalSamples(batch); batch = []; }
  }
  if (batch.length) await writeStatisticalSamples(batch);
  const current = await readStatisticsCache(record.id) ?? cache;
  current.locals = [...current.locals.filter(l => l.descriptor.id !== id), { descriptor, summary }];
  await saveStatisticsCache(current); return summary;
}
export async function requireSeries(record: StoredResult, id: string) {
  const cache = await ensureStatistics(record);
  return cache.overview.series.find(s => s.id === id) ?? ensureLocal(record, cache, id);
}
export async function statisticalDistribution(record: StoredResult, summary: SeriesSummary): Promise<Distribution> {
  const count = summary.votesKnown, id = record.id, series = summary.id;
  const quantile = async (p: number) => {
    if (!count) return null;
    const r = quantileRanks(count, p), low = (await orderedValue(id, series, r.lower))!, high = r.lower === r.upper ? low : (await orderedValue(id, series, r.upper))!;
    return interpolated(low, high, r.fraction);
  };
  const [min, max, q1, median, q3, p05, p95] = await Promise.all([0, 1, .25, .5, .75, .05, .95].map(quantile));
  const bounds = count ? { low: Math.max(0, q1! - (q3! - q1!) * 1.5), high: Math.min(100, q3! + (q3! - q1!) * 1.5) } : undefined;
  const [whiskerLow, whiskerHigh] = count ? await Promise.all([orderedValue(id, series, 0, bounds), orderedValue(id, series, 0, bounds, 'prev')]) : [null, null];
  return { count, min, max, mean: count ? summary.voteSum / count : null, q1, median, q3, p05, p95, whiskerLow, whiskerHigh };
}
/** Retain only the ranks needed for exact filtered quantiles, not the full series. */
async function filteredDistribution(record: StoredResult, id: string, simulations: ReadonlySet<number>) {
  let count = 0, sum = 0;
  for await (const p of samples(record.id, id)) if (simulations.has(p.simulation) && p.x !== null) { count++; sum += p.x; }
  const probabilities = [0, 1, .25, .5, .75, .05, .95, ...Array.from({ length: 11 }, (_, i) => i / 10)];
  const ranks = new Set(probabilities.flatMap(p => { const r = quantileRanks(count, p); return [r.lower, r.upper]; }));
  const values = new Map<number, number>();
  if (count) {
    let rank = 0;
    for await (const p of samplesByValue(record.id, id)) if (simulations.has(p.simulation)) {
      if (ranks.has(rank)) values.set(rank, p.x!);
      rank++;
    }
  }
  const quantile = (p: number) => {
    if (!count) return null;
    const r = quantileRanks(count, p);
    return interpolated(values.get(r.lower)!, values.get(r.upper)!, r.fraction);
  };
  const distribution: Distribution = { count, min: quantile(0), max: quantile(1), mean: count ? sum / count : null,
    q1: quantile(.25), median: quantile(.5), q3: quantile(.75), p05: quantile(.05), p95: quantile(.95), whiskerLow: null, whiskerHigh: null };
  return { distribution, deciles: count ? Array.from({ length: 11 }, (_, i) => quantile(i / 10)!) : [] };
}
export async function seriesStatistics(record: StoredResult, id: string, measure: 'elected' | 'entitled' = 'elected', bonusFilter: BonusFilter = 'all'): Promise<SeriesStatistics> {
  if (!['all', 'awarded', 'not-awarded'].includes(bonusFilter)) throw new Error('Invalid bonus filter');
  const summary = await requireSeries(record, id);
  let simulations: Set<number> | undefined;
  if (bonusFilter !== 'all') {
    simulations = new Set<number>();
    // Use the joint legal decision, never the selected list's filled bonus seats.
    // Missing/non-computable decisions belong to neither filtered subset.
    for await (const row of tableRows(record, 'Reform runs')) if (row.Bonus === bonusFilter) simulations.add(Number(row.Simulation));
  }
  const filtered = simulations ? await filteredDistribution(record, id, simulations) : undefined;
  const distribution = filtered?.distribution ?? await statisticalDistribution(record, summary);
  const boundaries: number[] = [];
  if (filtered) boundaries.push(...new Set(filtered.deciles));
  else for (let i = 0; i <= 10 && distribution.count; i++) {
    const r = quantileRanks(distribution.count, i / 10), a = (await orderedValue(record.id, id, r.lower))!, b = (await orderedValue(record.id, id, r.upper))!;
    const v = interpolated(a, b, r.fraction); if (boundaries.at(-1) !== v) boundaries.push(v);
  }
  if (boundaries.length === 1) boundaries.push(boundaries[0]);
  const bins = Array.from({ length: Math.max(0, boundaries.length - 1) }, (_, i) => ({ low: boundaries[i], high: boundaries[i + 1], count: 0, outcomes: [] as { seats: number | null; count: number }[] }));
  let known = 0, missingY = 0, yMax = 0;
  for await (const p of samples(record.id, id)) {
    if (p.x === null || simulations && !simulations.has(p.simulation)) continue;
    if (filtered) {
      const spread = (distribution.q3! - distribution.q1!) * 1.5;
      if (p.x >= distribution.q1! - spread && p.x <= distribution.q3! + spread) {
        distribution.whiskerLow = Math.min(distribution.whiskerLow ?? p.x, p.x);
        distribution.whiskerHigh = Math.max(distribution.whiskerHigh ?? p.x, p.x);
      }
    }
    const y = measure === 'entitled' ? p.entitled : p.y, bin = bins[binFor(p.x, boundaries)];
    if (y === null) missingY++; else { known++; yMax = Math.max(yMax, y); }
    bin.count++; const outcome = bin.outcomes.find(o => o.seats === y);
    if (outcome) outcome.count++; else bin.outcomes.push({ seats: y, count: 1 });
  }
  for (const bin of bins) bin.outcomes.sort((a, b) => (a.seats ?? Infinity) - (b.seats ?? Infinity));
  const requested = simulations?.size ?? record.result.benchmark.simulations;
  return { descriptor: summary, requested, known, missingX: requested - distribution.count, missingY, distribution, bins, yMax };
}
