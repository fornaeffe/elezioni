import { statisticalResult } from './statistical-result';
import { bundledReformScenario } from '../../src/lib/politics/reform-2026/application-defaults';
import { handleResultService } from '../../src/lib/workers/result-service';
import { getStoredResult, deleteStoredResult } from '../../src/lib/core/result-store';
import { streamStoredResult } from '../../src/lib/core/result-export';
import { ensureStatistics, seriesStatistics } from '../../src/lib/politics/reform-2026/stored-statistics';
import { readStatisticsCache, samplePage, deleteStatisticsCache } from '../../src/lib/core/statistics-cache';

self.onmessage = async () => {
  let id = '';
  try {
    const scenario = bundledReformScenario(), payload = statisticalResult(scenario), owner = 'statistics-test-owner';
    const runs = payload.result.tables.find(t => t.name === 'Reform runs')!;
    runs.columns.push('Bonus');
    for (const row of runs.rows) row.push(Number(row[0]) === 1201 ? 'non-computable' : Number(row[0]) % 2 ? 'awarded' : 'not-awarded');
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
    self.postMessage({ ok: true, overview: cache.overview, stats, college, entitlement, filtered, filteredEntitlement, pages, foreignDenied: !foreign.ok, archiveUnchanged: before.join('') === afterExport.join(''), removed,
      cacheFailure: !cacheFailure?.ok, canonicalRetained, partialCacheRemoved, rebuiltKnown: rebuilt.overview.series[0].votesKnown });
  } catch (e) { self.postMessage({ ok: false, error: String(e) }); }
};
