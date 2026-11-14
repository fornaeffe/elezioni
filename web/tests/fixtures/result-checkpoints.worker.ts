/** Storage-only opaque payloads: file/algorithm validation is tested by the
 * common lifecycle archive suite. Here test real IDB migration and atomicity. */
import { getStoredResult, saveStoredResult, deleteStoredResult, newResultStorage, withStoredFittedState, type StoredResult } from '../../src/lib/core/result-store';
import { createDefaultPoliticsScenario } from '../../src/lib/scenario/politics';

const request = <T>(r: IDBRequest<T>) => new Promise<T>((resolve, reject) => { r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); });
const done = (t: IDBTransaction) => new Promise<void>((resolve, reject) => { t.oncomplete = () => resolve(); t.onabort = () => reject(t.error); });
function record(id: string): StoredResult {
  return { id, owner: 'checkpoints-test', scenario: createDefaultPoliticsScenario(), exports: [], result: {
    type: 'result', status: 'not_implemented', tables: [], warnings: [], storage: newResultStorage(id),
    benchmark: { startedAt: '', elapsedMs: 0, simulations: 3, dataVersion: 'test' } } };
}
self.onmessage = () => { void (async () => {
  const common = record('legacy-common'), fast = record('legacy-fast');
  const checkpoint = { kind: 'politics-checkpoint', schemaVersion: 1, opaque: 'A'.repeat(100_000) } as unknown as NonNullable<StoredResult['politics']>['checkpoint'];
  const file = { kind: 'politics-reform-session', schemaVersion: 1, opaque: 'B'.repeat(100_000) } as unknown as NonNullable<StoredResult['fitFile']>;
  common.politics = { run: {} as NonNullable<StoredResult['politics']>['run'], checkpoint };
  fast.fitFile = file;
  const seed = indexedDB.open('elezioni-result-chunks-v1', 2);
  seed.onupgradeneeded = () => {
    const d = seed.result; d.createObjectStore('results', { keyPath: 'id' });
    const chunks = d.createObjectStore('chunks', { keyPath: ['resultId', 'table', 'sequence'] });
    chunks.createIndex('rows', ['resultId', 'table', 'start']); chunks.createIndex('runs', ['resultId', 'table', 'simulation', 'sequence']);
    const metadata = d.createObjectStore('metadata', { keyPath: ['resultId', 'table', 'ordinal'] });
    metadata.createIndex('signature', ['resultId', 'table', 'kind', 'text'], { unique: true });
  };
  const old = await request(seed), tx = old.transaction('results', 'readwrite'), seeded = done(tx);
  tx.objectStore('results').add(common); tx.objectStore('results').add(fast); await seeded; old.close();
  const lean = await getStoredResult(common.id, common.owner), leanFast = await getStoredResult(fast.id, fast.owner);
  const migrated = await withStoredFittedState(lean), migratedFast = await withStoredFittedState(leanFast);
  if (JSON.stringify(migrated.politics!.checkpoint) !== JSON.stringify(checkpoint) || JSON.stringify(migratedFast.fitFile) !== JSON.stringify(file)) throw new Error('Migration changed payload bytes');
  const db = await request(indexedDB.open('elezioni-result-chunks-v1'));
  const count = () => request(db.transaction('checkpoints').objectStore('checkpoints').count());
  const before = await count();
  let writes = 0;
  const add = IDBObjectStore.prototype.add, put = IDBObjectStore.prototype.put;
  IDBObjectStore.prototype.add = function(value, key) { if (this.name === 'checkpoints') writes++; return key === undefined ? add.call(this, value) : add.call(this, value, key); };
  IDBObjectStore.prototype.put = function(value, key) { if (this.name === 'checkpoints') writes++; return key === undefined ? put.call(this, value) : put.call(this, value, key); };
  try { for (let i = 1; i <= 3; i++) { lean.result.completedSimulations = i; await saveStoredResult(lean); } }
  finally { IDBObjectStore.prototype.add = add; IDBObjectStore.prototype.put = put; }
  const fresh = record('quota-common'); fresh.politics = { run: common.politics.run, checkpoint };
  IDBObjectStore.prototype.add = function(value, key) {
    if (this.name === 'checkpoints') throw new DOMException('Injected checkpoint quota failure', 'QuotaExceededError');
    return key === undefined ? add.call(this, value) : add.call(this, value, key);
  };
  let quota = false, invisible = false;
  try { await saveStoredResult(fresh); } catch (e) { quota = e instanceof DOMException && e.name === 'QuotaExceededError'; }
  finally { IDBObjectStore.prototype.add = add; }
  try { await getStoredResult(fresh.id); } catch { invisible = true; }
  const orphanFree = await count() === before;
  await saveStoredResult(fresh);
  const saved = await getStoredResult(fresh.id), restored = await withStoredFittedState(saved);
  let immutable = false;
  try { await saveStoredResult(restored); } catch { immutable = true; }
  await deleteStoredResult(fresh.id, fresh.owner); await deleteStoredResult(common.id, common.owner); await deleteStoredResult(fast.id, fast.owner);
  const remaining = await count(); db.close();
  self.postMessage({ ok: true, version: 3, before, writes, lean: !lean.politics!.checkpoint && !leanFast.fitFile,
    manifestBytes: JSON.stringify(lean).length, quota, invisible, orphanFree, immutable, remaining,
    resumedPayload: JSON.stringify(restored.politics!.checkpoint) === JSON.stringify(checkpoint) });
})().catch(e => self.postMessage({ ok: false, error: String(e) })); };
