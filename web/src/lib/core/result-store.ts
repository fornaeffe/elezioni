import { gzipSync, gunzipSync } from 'fflate';
import type { ExportScenario } from './result-export';
import type { ResultTable, SimulationResult, ScrutinyWarning } from './types';
import { deleteStatisticsCache } from './statistics-cache';

export type Cell = string | number | boolean | null;
export interface StoredTable { name: string; columns: string[]; rows: number; chunks: number; shared?: boolean; runIndexed?: boolean }
export interface ResultStorage {
  id: string;
  state: 'writing' | 'complete' | 'interrupted';
  tables: StoredTable[];
  completedSimulations: number;
  compressedBytes: number;
  maxChunkBytes: number;
  warningCounts: { info: number; warning: number; error: number };
}
export interface StoredResult {
  fitFile?: import('../politics/reform-2026/vote-share/checkpoint').PortableFitFile;
  id: string;
  owner: string;
  scenario: ExportScenario;
  result: SimulationResult;
  exports: string[];
  metadataCounts?: Record<number, number[]>;
}
interface Chunk {
  resultId: string; table: number; sequence: number; start: number; count: number;
  simulation: number; bytes: Uint8Array;
}
export const warningColumns = ['code', 'electionKind', 'simulationId', 'severity', 'message', 'lawReference', 'todoReference', 'chamber', 'interpretationId', 'seatId', 'drawContextId', 'scope', 'occurrences', 'reason'];
export const resultChunkBytes = 256 * 1024;
export const resultChunkRows = 256;
export const maximumResultRecordBytes = 16 * 1024 * 1024;
const encoder = new TextEncoder(), decoder = new TextDecoder();
let database: Promise<IDBDatabase> | undefined;
const pendingMetadata = new WeakMap<StoredResult, { table: number; rows: Cell[][] }[]>();
// Only distinct shared context keys are cached, never per-simulation details.
// The cache dies with the calculation; persisted counters are a small vector.
const metadataOrdinals = new WeakMap<StoredResult, Map<string, number>>();

function db(): Promise<IDBDatabase> {
  return database ??= new Promise((resolve, reject) => {
    const request = indexedDB.open('elezioni-result-chunks-v1', 2);
    request.onupgradeneeded = () => {
      const d = request.result;
      if (!d.objectStoreNames.contains('results')) d.createObjectStore('results', { keyPath: 'id' });
      if (!d.objectStoreNames.contains('chunks')) {
        const chunks = d.createObjectStore('chunks', { keyPath: ['resultId', 'table', 'sequence'] });
        chunks.createIndex('rows', ['resultId', 'table', 'start']);
        chunks.createIndex('runs', ['resultId', 'table', 'simulation', 'sequence']);
      }
      if (!d.objectStoreNames.contains('metadata')) {
        const metadata = d.createObjectStore('metadata', { keyPath: ['resultId', 'table', 'ordinal'] });
        metadata.createIndex('signature', ['resultId', 'table', 'kind', 'text'], { unique: true });
      }
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => { database = undefined; reject(request.error); };
  });
}
function value<T>(request: IDBRequest<T>): Promise<T> {
  return new Promise((resolve, reject) => { request.onsuccess = () => resolve(request.result); request.onerror = () => reject(request.error); });
}
function committed(transaction: IDBTransaction): Promise<void> {
  return new Promise((resolve, reject) => {
    transaction.oncomplete = () => resolve();
    transaction.onabort = () => reject(transaction.error ?? new Error('Result storage transaction aborted'));
    transaction.onerror = () => { /* onabort reports the transaction failure */ };
  });
}
export async function getStoredResult(id: string, owner?: string): Promise<StoredResult> {
  const record = await value((await db()).transaction('results').objectStore('results').get(id)) as StoredResult | undefined;
  if (!record || owner !== undefined && record.owner !== owner) throw new Error('Local result session is unavailable');
  return record;
}
/** Full-result and ID exports can finish concurrently. Register each temporary
 * file atomically without replacing the other export's manifest update. */
export async function registerStoredExport(id: string, owner: string, filename: string): Promise<void> {
  const tx = (await db()).transaction('results', 'readwrite'), store = tx.objectStore('results');
  const record = await value(store.get(id)) as StoredResult | undefined;
  if (!record || record.owner !== owner) throw new Error('Local result session is unavailable');
  const done = committed(tx);
  record.exports.push(filename); store.put(record); await done;
}
export async function saveStoredResult(record: StoredResult): Promise<void> {
  const pending = pendingMetadata.get(record) ?? [];
  const tx = (await db()).transaction(['results', 'metadata'], 'readwrite'), done = committed(tx);
  const ordinals = metadataOrdinals.get(record) ?? new Map<string, number>();
  metadataOrdinals.set(record, ordinals);
  for (const batch of pending) for (const cells of batch.rows) {
    const descriptor = record.result.storage!.tables[batch.table];
    const signature = JSON.stringify([batch.table, cells[0], cells[1]]);
    let ordinal = ordinals.get(signature);
    if (ordinal === undefined) {
      ordinal = descriptor.rows++; ordinals.set(signature, ordinal);
      tx.objectStore('metadata').add({ resultId: record.id, table: batch.table, ordinal, kind: cells[0], text: cells[1], cells });
    }
    const counts = (record.metadataCounts ??= {})[batch.table] ??= [];
    counts[ordinal] = (counts[ordinal] ?? 0) + Number(cells[2]);
  }
  tx.objectStore('results').put(record);
  await done; pendingMetadata.delete(record);
}
export function newResultStorage(id: string): ResultStorage {
  return { id, state: 'writing', tables: [], completedSimulations: 0, compressedBytes: 0, maxChunkBytes: 0, warningCounts: { info: 0, warning: 0, error: 0 } };
}
export function warningCells(w: ScrutinyWarning): Cell[] {
  return warningColumns.map(c => (w as unknown as Record<string, Cell>)[c] ?? null);
}
export function cellsWarning(cells: Cell[]): ScrutinyWarning {
  return Object.fromEntries(warningColumns.flatMap((c, i) => cells[i] == null ? [] : [[c, cells[i]]])) as unknown as ScrutinyWarning;
}

/** One bounded buffer. Encoding is columnar at the storage boundary, not after
 * building a second full result. A single oversized trace fails explicitly. */
export class ResultChunkWriter {
  private rows: Cell[][] = [];
  private bytes = 2;
  private simulation = 0;
  constructor(readonly record: StoredResult, readonly table: number) {}
  async append(row: Cell[], simulation = 0): Promise<void> {
    const size = encoder.encode(JSON.stringify(row)).byteLength + 1;
    if (size > maximumResultRecordBytes) throw new Error(`Result record exceeds ${maximumResultRecordBytes} bytes`);
    if (this.rows.length && (this.bytes + size > resultChunkBytes || this.rows.length >= resultChunkRows || simulation !== this.simulation)) await this.flush();
    this.rows.push(row); this.bytes += size; this.simulation = simulation;
  }
  async flush(): Promise<void> {
    if (!this.rows.length) return;
    const storage = this.record.result.storage!, descriptor = this.table === -1 ? null : storage.tables[this.table];
    const sequence = descriptor?.chunks ?? (this.record as StoredResult & { warningChunks?: number }).warningChunks ?? 0;
    const raw = encoder.encode(JSON.stringify(this.rows));
    const chunk: Chunk = { resultId: this.record.id, table: this.table, sequence, start: descriptor?.rows ?? 0,
      count: this.rows.length, simulation: this.simulation, bytes: gzipSync(raw, { level: 1, mtime: 0 }) };
    // Mutate the manifest only after the chunk transaction commits.
    const tx = (await db()).transaction('chunks', 'readwrite'), done = committed(tx);
    tx.objectStore('chunks').add(chunk); await done;
    if (descriptor) { descriptor.rows += chunk.count; descriptor.chunks++; }
    else (this.record as StoredResult & { warningChunks?: number }).warningChunks = sequence + 1;
    storage.compressedBytes += chunk.bytes.byteLength;
    storage.maxChunkBytes = Math.max(storage.maxChunkBytes, raw.byteLength);
    if (descriptor) descriptor.runIndexed = descriptor.runIndexed !== false && this.simulation > 0;
    this.rows = []; this.bytes = 2;
  }
}
export async function appendResultBatch(record: StoredResult, tables: ResultTable[], warnings: ScrutinyWarning[], simulation: number): Promise<void> {
  const storage = record.result.storage!;
  for (let index = 0; index < tables.length; index++) {
    const table = tables[index];
    storage.tables[index] ??= { name: table.name, columns: table.columns, rows: 0, chunks: 0 };
    if (table.name === 'Models, references and conditions') {
      storage.tables[index].shared = true;
      if (table.rows.length) {
        const pending = pendingMetadata.get(record) ?? [];
        pending.push({ table: index, rows: table.rows.map(row => table.columns.map(c => row[c] ?? null)) });
        pendingMetadata.set(record, pending);
      }
      continue;
    }
    const writer = new ResultChunkWriter(record, index);
    for (const row of table.rows) await writer.append(table.columns.map(c => row[c] ?? null), Number(row.Simulation ?? simulation));
    await writer.flush();
  }
  const writer = new ResultChunkWriter(record, -1);
  for (const w of warnings) {
    await writer.append(warningCells(w), w.simulationId ?? simulation);
    storage.warningCounts[w.severity ?? 'warning']++;
  }
  await writer.flush();
}
async function nextChunk(id: string, table: number, sequence: number, simulation?: number): Promise<Chunk | undefined> {
  const tx = (await db()).transaction('chunks'), store = tx.objectStore('chunks');
  const range = simulation === undefined
    ? IDBKeyRange.bound([id, table, sequence], [id, table, Number.MAX_SAFE_INTEGER])
    : IDBKeyRange.bound([id, table, simulation, sequence], [id, table, simulation, Number.MAX_SAFE_INTEGER]);
  const cursor = await value((simulation === undefined ? store : store.index('runs')).openCursor(range));
  return cursor?.value as Chunk | undefined;
}
export function decodeResultChunk(bytes: Uint8Array): Cell[][] {
  return JSON.parse(decoder.decode(gunzipSync(bytes))) as Cell[][];
}
export async function* storedRows(id: string, table: number, options: { offset?: number; simulation?: number } = {}): AsyncGenerator<Cell[]> {
  const published = await getStoredResult(id);
  const descriptor = published.result.storage!.tables[table];
  if (descriptor?.shared) {
    let ordinal = options.offset ?? 0;
    while (ordinal < descriptor.rows) {
      const tx = (await db()).transaction('metadata');
      const current = ordinal++;
      const row = await value(tx.objectStore('metadata').get([id, table, current]));
      if (row) {
        const cells = row.cells as Cell[];
        cells[2] = published.metadataCounts?.[table]?.[current] ?? cells[2];
        yield cells;
      }
    }
    return;
  }
  const maximumChunks = table === -1 ? (published as StoredResult & { warningChunks?: number }).warningChunks ?? 0 : published.result.storage!.tables[table]?.chunks ?? 0;
  let sequence = 0;
  if (options.offset && options.simulation === undefined) {
    const index = (await db()).transaction('chunks').objectStore('chunks').index('rows');
    const cursor = await value(index.openCursor(IDBKeyRange.bound([id, table, 0], [id, table, options.offset]), 'prev'));
    sequence = (cursor?.value as Chunk | undefined)?.sequence ?? 0;
  }
  for (;;) {
    const chunk = await nextChunk(id, table, sequence, options.simulation);
    if (!chunk || chunk.sequence >= maximumChunks) return;
    sequence = chunk.sequence + 1;
    if (options.offset && chunk.start + chunk.count <= options.offset) continue;
    const rows = decodeResultChunk(chunk.bytes);
    const skip = options.offset ? Math.max(0, options.offset - chunk.start) : 0;
    for (let i = skip; i < rows.length; i++) yield rows[i];
  }
}
export async function storedTablePage(record: StoredResult, index: number, offset: number, limit = 50, simulation?: number): Promise<ResultTable> {
  if (!Number.isSafeInteger(offset) || offset < 0 || !Number.isSafeInteger(limit) || limit < 1 || limit > 1000) throw new Error('Invalid result page');
  const descriptor = record.result.storage!.tables[index];
  if (!descriptor) throw new Error('Unknown result table');
  const rows: ResultTable['rows'] = [];
  const useIndex = simulation !== undefined && descriptor.runIndexed !== false;
  for await (const cells of storedRows(record.id, index, { offset, simulation: useIndex ? simulation : undefined })) {
    if (simulation !== undefined && !useIndex && cells[descriptor.columns.indexOf('Simulation')] !== simulation) continue;
    rows.push(Object.fromEntries(descriptor.columns.map((c, i) => [c, cells[i]])));
    if (rows.length === limit) break;
  }
  return { name: descriptor.name, columns: descriptor.columns, rows };
}
export async function storedWarningPage(record: StoredResult, offset: number, limit = 10): Promise<ScrutinyWarning[]> {
  const result: ScrutinyWarning[] = [];
  for (const severity of ['error', 'warning'] as const) {
    for await (const cells of storedRows(record.id, -1)) {
      const w = cellsWarning(cells);
      if ((w.severity ?? 'warning') !== severity) continue;
      if (offset > 0) { offset--; continue; }
      result.push(w); if (result.length === limit) return result;
    }
  }
  return result;
}
export async function deleteStoredResult(id: string, owner: string): Promise<void> {
  const record = await getStoredResult(id, owner), d = await db();
  const tx = d.transaction(['results', 'chunks', 'metadata'], 'readwrite'), done = committed(tx);
  tx.objectStore('results').delete(id);
  tx.objectStore('chunks').delete(IDBKeyRange.bound([id, -1, 0], [id, Number.MAX_SAFE_INTEGER, Number.MAX_SAFE_INTEGER]));
  tx.objectStore('metadata').delete(IDBKeyRange.bound([id, 0, 0], [id, Number.MAX_SAFE_INTEGER, Number.MAX_SAFE_INTEGER]));
  await done;
  await deleteStatisticsCache(id);
  if (record.exports.length && navigator.storage?.getDirectory) {
    const root = await navigator.storage.getDirectory();
    const directory = await root.getDirectoryHandle('elezioni-result-exports', { create: true });
    for (const file of record.exports) await directory.removeEntry(file).catch(() => undefined);
  }
}
