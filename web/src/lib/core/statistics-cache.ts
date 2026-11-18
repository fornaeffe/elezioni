import type { SeriesDescriptor, SeriesSummary, StatisticalSample, StatisticsOverview } from '../results/statistics';

// Rebuildable derived data lives separately from canonical archives. Its
// publication marker is written only after all bounded sample batches commit.
let database: Promise<IDBDatabase> | undefined;
function db() {
  return database ??= new Promise<IDBDatabase>((resolve, reject) => {
    const request = indexedDB.open('elezioni-statistics-v1', 1);
    request.onupgradeneeded = () => {
      const d = request.result;
      d.createObjectStore('catalogues', { keyPath: 'id' });
      const samples = d.createObjectStore('samples', { keyPath: ['resultId', 'series', 'simulation'] });
      samples.createIndex('values', ['resultId', 'series', 'x', 'simulation']);
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => { database = undefined; reject(request.error); };
  });
}
function value<T>(r: IDBRequest<T>): Promise<T> { return new Promise((resolve, reject) => { r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); }); }
function committed(tx: IDBTransaction) { return new Promise<void>((resolve, reject) => { tx.oncomplete = () => resolve(); tx.onabort = () => reject(tx.error); tx.onerror = () => {}; }); }
export interface StatisticsCache { id: string; fingerprint: string; overview: StatisticsOverview; locals: { descriptor: SeriesDescriptor; summary: SeriesSummary }[] }
export async function readStatisticsCache(id: string): Promise<StatisticsCache | undefined> {
  return value((await db()).transaction('catalogues').objectStore('catalogues').get(id));
}
export async function saveStatisticsCache(cache: StatisticsCache) {
  const tx = (await db()).transaction('catalogues', 'readwrite'), done = committed(tx);
  tx.objectStore('catalogues').put(cache); await done;
}
export async function writeStatisticalSamples(samples: StatisticalSample[]) {
  const tx = (await db()).transaction('samples', 'readwrite'), done = committed(tx);
  try { for (const sample of samples) tx.objectStore('samples').put(sample); await done; }
  catch (error) { try { tx.abort(); } catch { /* already completed */ } await done.catch(() => undefined); throw error; }
}
export async function deleteStatisticsCache(id: string) {
  const tx = (await db()).transaction(['catalogues', 'samples'], 'readwrite'), done = committed(tx);
  tx.objectStore('catalogues').delete(id);
  tx.objectStore('samples').delete(IDBKeyRange.bound([id, '', 0], [id, '\uffff', Number.MAX_SAFE_INTEGER])); await done;
}
/** Fixed-size pages; duplicates retain their simulation ID and multiplicity. */
export async function samplePage(id: string, series: string, after = 0, limit = 256): Promise<StatisticalSample[]> {
  if (!Number.isSafeInteger(after) || after < 0 || !Number.isSafeInteger(limit) || limit < 1 || limit > 256) throw new RangeError('Invalid statistics page');
  const store = (await db()).transaction('samples').objectStore('samples');
  return value(store.getAll(IDBKeyRange.bound([id, series, after], [id, series, Number.MAX_SAFE_INTEGER], true), limit));
}
export async function* samples(id: string, series: string): AsyncGenerator<StatisticalSample> {
  let after = 0;
  for (;;) {
    const page = await samplePage(id, series, after); if (!page.length) return;
    for (const sample of page) yield sample;
    after = page.at(-1)!.simulation;
  }
}
/** Stream the value index in bounded pages, retaining equal values by simulation. */
export async function* samplesByValue(id: string, series: string): AsyncGenerator<StatisticalSample> {
  let after: IDBValidKey = [id, series, 0, 0];
  for (;;) {
    const index = (await db()).transaction('samples').objectStore('samples').index('values');
    const page: StatisticalSample[] = await value(index.getAll(IDBKeyRange.bound(after, [id, series, 100, Number.MAX_SAFE_INTEGER], true), 256));
    if (!page.length) return;
    for (const sample of page) yield sample;
    const last = page.at(-1)!; after = [id, series, last.x!, last.simulation];
  }
}
export async function orderedValue(id: string, series: string, rank: number, bounds?: { low: number; high: number }, direction: IDBCursorDirection = 'next'): Promise<number | null> {
  const index = (await db()).transaction('samples').objectStore('samples').index('values');
  const range = IDBKeyRange.bound([id, series, bounds?.low ?? 0, 0], [id, series, bounds?.high ?? 100, Number.MAX_SAFE_INTEGER]);
  return new Promise((resolve, reject) => {
    const request = index.openCursor(range, direction); let advanced = false;
    request.onerror = () => reject(request.error);
    request.onsuccess = () => {
      const cursor = request.result;
      if (!cursor) resolve(null);
      else if (!advanced && rank > 0) { advanced = true; cursor.advance(rank); }
      else resolve((cursor.value as StatisticalSample).x);
    };
  });
}
