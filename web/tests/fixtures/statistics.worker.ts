import { statisticalResult } from './statistical-result';
import { bundledReformScenario } from '../../src/lib/politics/reform-2026/application-defaults';
import { handleResultService } from '../../src/lib/workers/result-service';
import { getStoredResult, deleteStoredResult } from '../../src/lib/core/result-store';
import { streamStoredResult } from '../../src/lib/core/result-export';
import { ensureStatistics, seriesStatistics } from '../../src/lib/politics/reform-2026/stored-statistics';
import { readStatisticsCache, samplePage, deleteStatisticsCache } from '../../src/lib/core/statistics-cache';
import { electionPositionColumns, positionColumns, positionTable } from '../../src/lib/politics/reform-2026/election-probability';

self.onmessage = async () => {
  let id = '';
  try {
    const scenario = bundledReformScenario(), payload = statisticalResult(scenario), owner = 'statistics-test-owner';
    const runs = payload.result.tables.find(t => t.name === 'Reform runs')!;
    runs.columns.push('Bonus');
    for (const row of runs.rows) row.push(Number(row[0]) === 1201 ? 'non-computable' : Number(row[0]) % 2 ? 'awarded' : 'not-awarded');
    const locals = payload.result.tables.find(t => t.name === 'Reform college series v1')!;
    const source = locals.rows.filter(r => r[locals.columns.indexOf('College')] === 'constructed-1');
    for (const row of source) for (const axis of ['x', 'y']) {
      const simulation = Number(row[0]);
      if (simulation === (axis === 'x' ? 2 : 3)) continue;
      const values = Object.fromEntries(locals.columns.map((c, i) => [c, row[i]]));
      Object.assign(values, { Owner: `list:paired-${axis}`, College: `paired-${axis}`, Name: `Paired ${axis}`,
        'Vote share %': axis === 'x' ? simulation === 7 ? null : simulation % 101 : 50,
        Elected: axis === 'x' ? 99 : simulation === 5 ? null : simulation % 4, Entitled: axis === 'x' ? 98 : simulation % 3 });
      locals.rows.push(locals.columns.map(c => values[c]));
    }
    locals.rows.sort((a, b) => Number(a[0]) - Number(b[0]));
    const positionRows = [];
    for (let simulation = 1; simulation <= 1201; simulation++) {
      if (simulation === 3 || simulation === 9) continue;
      const last = simulation % 8;
      const values: (number | null)[] = positionColumns.map((_, i) => Number(i + 1 === last));
      if (simulation === 5) { values.fill(0); values[0] = 1; values[6] = null; }
      if (simulation === 11) { values.fill(0); values[0] = null; values[2] = 1; }
      if (simulation === 13) values.fill(null);
      if (simulation === 17) { values.fill(null); values[6] = 1; }
      positionRows.push([simulation, 'camera', 'paired-y', 'paired-y', 'Paired y', ...values]);
    }
    payload.result.tables.push({ name: positionTable, columns: electionPositionColumns, rows: positionRows });
    id = crypto.randomUUID();
    const imported = await handleResultService({ type: 'result-service', requestId: 1, action: 'import', resultId: id, owner, file: new File([JSON.stringify(payload)], 'statistics.json') });
    if (!imported.ok) throw new Error(imported.error!.message);
    const record = await getStoredResult(id, owner);
    const before: string[] = []; for await (const text of streamStoredResult(record, 'csv')) before.push(text);
    const cache = await ensureStatistics(record), summary = cache.overview.series.find(s => s.kind === 'list')!;
    const stats = await seriesStatistics(record, summary.id), college = await seriesStatistics(record, cache.overview.colleges[0].id), entitlement = await seriesStatistics(record, cache.overview.colleges[0].id, 'entitled');
    const filtered = {} as Record<string, unknown>;
    for (const filter of ['awarded', 'not-awarded'] as const) {
      const response = await handleResultService({ type: 'result-service', requestId: 10, action: 'statistics-series', resultId: id, owner, series: summary.id, bonusFilter: filter });
      if (!response.ok) throw new Error(response.error!.message);
      filtered[filter] = response.data;
    }
    const filteredEntitlement = await seriesStatistics(record, cache.overview.colleges[0].id, 'entitled', 'awarded');
    const ySeries = cache.overview.colleges.find(c => c.college === 'paired-y')!.id, xSeries = cache.overview.colleges.find(c => c.college === 'paired-x')!.id;
    const paired = {} as Record<string, unknown>;
    for (const filter of ['all', 'awarded', 'not-awarded'] as const) {
      const response = await handleResultService({ type: 'result-service', requestId: 11, action: 'statistics-series', resultId: id, owner, series: ySeries, xSeries, bonusFilter: filter });
      if (!response.ok) throw new Error(response.error!.message);
      paired[filter] = response.data;
    }
    const pairedEntitlement = await seriesStatistics(record, ySeries, 'entitled', 'awarded', xSeries);
    const lastPositions: Record<string, unknown> = {};
    for (const filter of ['all', 'awarded', 'not-awarded'] as const) {
      const response = await handleResultService({ type: 'result-service', requestId: 12, action: 'statistics-series', resultId: id, owner, series: ySeries, xSeries, measure: 'last-position', bonusFilter: filter });
      if (!response.ok) throw new Error(response.error!.message);
      lastPositions[filter] = response.data;
    }
    const noSlate = await seriesStatistics(record, cache.overview.colleges[0].id, 'last-position');
    const invalidLastPosition = await handleResultService({ type: 'result-service', requestId: 13, action: 'statistics-series', resultId: id, owner, series: summary.id, measure: 'last-position' });
    const pages = []; let after = 0;
    for (;;) { const page = await samplePage(id, summary.id, after); if (!page.length) break; pages.push({ length: page.length, first: page[0].simulation, last: page.at(-1)!.simulation }); after = page.at(-1)!.simulation; }
    const foreign = await handleResultService({ type: 'result-service', requestId: 2, action: 'statistics', resultId: id, owner: 'foreign' });
    const afterExport: string[] = []; for await (const text of streamStoredResult(record, 'csv')) afterExport.push(text);
    await deleteStatisticsCache(id);
    const original = IDBObjectStore.prototype.put; let writes = 0;
    IDBObjectStore.prototype.put = function(value, key) {
      if (this.name === 'samples' && ++writes === 260) throw new DOMException('Derived cache quota', 'QuotaExceededError');
      return key === undefined ? original.call(this, value) : original.call(this, value, key);
    };
    let cacheFailure;
    try { cacheFailure = await handleResultService({ type: 'result-service', requestId: 3, action: 'statistics', resultId: id, owner }); }
    finally { IDBObjectStore.prototype.put = original; }
    const canonicalRetained = (await getStoredResult(id, owner)).result.status === 'completed';
    const partialCacheRemoved = await readStatisticsCache(id) === undefined && (await samplePage(id, summary.id)).length === 0;
    // Rebuilding after a cache failure never needs a legal rerun.
    const rebuilt = await ensureStatistics(record);
    await deleteStoredResult(id, owner);
    const removed = await readStatisticsCache(id) === undefined && (await samplePage(id, summary.id)).length === 0;
    self.postMessage({ ok: true, overview: cache.overview, stats, college, entitlement, filtered, filteredEntitlement, paired, pairedEntitlement, lastPositions, noSlate, invalidLastPositionDenied: !invalidLastPosition.ok, pages, foreignDenied: !foreign.ok, archiveUnchanged: before.join('') === afterExport.join(''), removed,
      cacheFailure: !cacheFailure?.ok, canonicalRetained, partialCacheRemoved, rebuiltKnown: rebuilt.overview.series[0].votesKnown });
  } catch (e) { self.postMessage({ ok: false, error: String(e) }); }
};
